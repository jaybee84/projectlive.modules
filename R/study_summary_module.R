# Study Summary Module UI

#' @title   study_summary_module_ui and study_summary_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname study_summary_module
#' @export
study_summary_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
          shinydashboard::box(
            shiny::textOutput(ns('header_text')),
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary"
          ),
          study_selection_module_ui(ns("study_summary")),
          shinydashboard::box(
            shiny::htmlOutput(ns('study_summary')),
            title = "Study Summary",
            status = "primary",
            solidHeader = T,
            width = 12,
            collapsible = FALSE
          ),
          plot_module_ui(ns("study_timeline_plot"), "Study Timeline"),
          shinydashboard::box(
            plotly::plotlyOutput(ns('data_focus_plot')),
            title = "Data Focus",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE
          ),
          plot_module_ui(ns("annotation_activity_plot"), "Annotation Activity"),
          plot_module_ui(ns("publication_status_plot"), "Publication Status"),
          shinydashboard::box(
            title = "test",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            DT::dataTableOutput(ns("dt")),
            shiny::numericInput(
              inputId = ns("days_choice"),
              label = "Select Amount of Days",
              value = 1000L,
              min = 0L,
              step = 1L
            ),
            plotly::plotlyOutput(ns("plot"))
          )
        )
      )
    )
  )
}

# Study Summary Module Server

#' @title study_summary_module_server and study_summary_module_server_ui
#' @param data A named list. The list must contain a list named "tables".
#' @param config A named list. The list must contain lists for each section:
#' - header_text
#' - study_table
#' - study_summary
#' - data_focus_plot
#' - annotation_activity_plot
#' - publication_status_plot
#' @rdname study_summary_module
#' @export
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
study_summary_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$header_text <- shiny::renderText({
        shiny::req(config())
        glue::glue(config()$header_text)
      })

      filtered_data <- study_selection_module_server(
        "study_summary", data, config
      )

      output$study_summary <- shiny::renderText({

        shiny::req(filtered_data(), config())

        config <- purrr::pluck(config(), "study_summary")

        data <- filtered_data() %>%
          purrr::pluck("tables", config$table) %>%
          format_plot_data_with_config(config) %>%
          dplyr::distinct() %>%
          dplyr::mutate("Unique Study ID" = stringr::str_c(
            '<a href=',
            '"https://www.synapse.org/#!Synapse:',
            .data$`Unique Study ID`,
            '">',
            .data$`Unique Study ID`,
            '</a>'
          )) %>%
          tidyr::pivot_longer(dplyr::everything()) %>%
          dplyr::mutate(
            "name" = stringr::str_to_title(.data$name),
            "name" = stringr::str_c("<b>", .data$name, "</b>")
          ) %>%
          knitr::kable(
            "html", escape = FALSE, col.names = NULL, align = c('r', 'l')
          ) %>%
          kableExtra::kable_styling("striped", full_width = T)
      })

      plot_module_server(
        id = "study_timeline_plot",
        data = filtered_data,
        config = shiny::reactive(purrr::pluck(config(), "study_timeline_plot")),
        plot_func = shiny::reactive("create_study_timeline_plot")
      )

      output$data_focus_plot <- plotly::renderPlotly({
        shiny::req(filtered_data(), config())

        config <- purrr::pluck(config(), "data_focus_plot")

        data_list <- filtered_data() %>%
          purrr::pluck("tables", config$table) %>%
          format_plot_data_with_config(config) %>%
          create_data_focus_tables(config$plot$x, config$plot$fill)

        shiny::validate(shiny::need(length(data_list) > 0 , config$empty_table_message))

        create_data_focus_plots(data_list, config)
      })

      plot_module_server(
        id = "annotation_activity_plot",
        data = filtered_data,
        config = shiny::reactive(
          purrr::pluck(config(), "annotation_activity_plot")
        ),
        plot_func = shiny::reactive("create_annotation_activity_plot")
      )

      plot_module_server(
        id = "publication_status_plot",
        data = filtered_data,
        config = shiny::reactive(
          purrr::pluck(config(), "publication_status_plot")
        ),
        plot_func = shiny::reactive("create_publication_status_plot")
      )

      files_tbl <- shiny::reactive({
        shiny::req(filtered_data(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot", "files_table")
        tbl <- purrr::pluck(filtered_data(), "tables", config$name)
        format_plot_data_with_config(tbl, config)
      })


      id_tbl <- shiny::reactive({
        shiny::req(filtered_data(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot", "incoming_data_table")
        tbl <- purrr::pluck(filtered_data(), "tables", config$name)
        format_plot_data_with_config(tbl, config)
      })

      dt_tbl <- shiny::reactive({
        id_tbl() %>%
          dplyr::select(Milestone, `Date Estimate`) %>%
          dplyr::distinct()
      })

      output$dt <- DT::renderDataTable(
        base::as.data.frame(dt_tbl()),
        server = TRUE,
        selection = 'single'
      )

      dt_row <- shiny::reactive({
        dt_tbl() %>%
          dplyr::slice(input$dt_rows_selected)
      })

      date_tbl <- shiny::reactive({
        dt_row() %>%
          dplyr::select(`Date Estimate`) %>%
          dplyr::mutate(
            "min_date" = `Date Estimate` - lubridate::duration(input$days_choice, 'days'),
            "max_date" = `Date Estimate` + lubridate::duration(input$days_choice, 'days')
          ) %>%
          dplyr::select("min_date", "max_date")
      })

      filtered_id_tbl <- shiny::reactive({
        id_tbl() %>%
          dplyr::inner_join(dt_row()) %>%
          dplyr::select(Format, Milestone, Expected)
      })

      filtered_files_tbl <- shiny::reactive({
        files_tbl() %>%
          print() %>%
          dplyr::filter(
            `Date Created` < date_tbl()$max_date,
            `Date Created` > date_tbl()$min_date
          ) %>%
          dplyr::group_by(Format) %>%
          dplyr::summarise("Actual" = dplyr::n())
      })

      merged_table <- shiny::reactive({
        filtered_id_tbl() %>%
          dplyr::full_join(filtered_files_tbl()) %>%
          dplyr::mutate(
            "Actual" = dplyr::if_else(
              is.na(.data$Actual),
              0L,
              .data$Actual
            )
          ) %>%
          dplyr::select(Format, Expected, Actual) %>%
          tidyr::pivot_longer(-"Format")
      })

      plot_obj <- shiny::reactive({
        merged_table() %>%
          ggplot2::ggplot() +
          ggplot2::geom_bar(
            ggplot2::aes(
              x = !!rlang::sym("Format"),
              y = !!rlang::sym("value"),
              fill = !!rlang::sym("name")
            ),
            stat = "identity",
            alpha = 0.8,
            na.rm = TRUE,
            show.legend = FALSE,
            position = ggplot2::position_dodge()
          )
      })

      output$plot <- plotly::renderPlotly({
        shiny::req(plot_obj())
        plot_obj()
      })

    }
  )
}
