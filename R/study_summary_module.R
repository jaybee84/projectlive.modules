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
            title = "Internal milestone tracker",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::numericInput(
                  inputId = ns("days_choice"),
                  label = "Select Amount of Days",
                  value = 60L,
                  min = 0L,
                  step = 1L
                )
              ),
              shiny::column(
                width = 6,
                DT::dataTableOutput(ns("dt1"))
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 12,
                plotly::plotlyOutput(ns("plot1"))
              )
            )
          ),
          shinydashboard::box(
            title = "Researcher reported milestone upload",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::uiOutput(ns("milestone_choice_ui"))
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                plotly::plotlyOutput(ns("plot2a"))
              ),
              shiny::column(
                width = 6,
                plotly::plotlyOutput(ns("plot2b"))
              )
            )
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
        shiny::validate(shiny::need(
          !is.null(purrr::pluck(config(), "milestone_reporting_plot")),
          "Not Tracking Milestones"
        ))
        config <- purrr::pluck(config(), "milestone_reporting_plot", "files_table")
        tbl <- purrr::pluck(filtered_data(), "tables", config$name)
        format_plot_data_with_config(tbl, config)
      })

      id_tbl <- shiny::reactive({
        shiny::req(filtered_data(), config())
        shiny::validate(shiny::need(
          !is.null(purrr::pluck(config(), "milestone_reporting_plot")),
          "Not Tracking Milestones"
        ))
        config <- purrr::pluck(config(), "milestone_reporting_plot", "incoming_data_table")
        tbl <- purrr::pluck(filtered_data(), "tables", config$name)
        shiny::validate(shiny::need(
          nrow(tbl) > 0,
          "Study has no current milestones."
        ))
        format_plot_data_with_config(tbl, config)
      })

      # plot1 ----

      dt_tbl1 <- shiny::reactive({
        shiny::req(id_tbl(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")
        date_column        <- rlang::sym(config$date_estimate_column)
        milestone_column   <- rlang::sym(config$milestone_column)

        tbl <- id_tbl()
          dplyr::filter(!is.na(!!date_column)) %>%
          dplyr::select(!!milestone_column, !!date_column) %>%
          dplyr::arrange(!!date_column) %>%
          dplyr::distinct() %>%
          dplyr::mutate(
            "Date Range Start" =
              !!date_column - lubridate::duration(input$days_choice, 'days'),
            "Date Range End" =
              !!date_column + lubridate::duration(input$days_choice, 'days')
          )
      })

      output$dt1 <- DT::renderDataTable(
        base::as.data.frame(dt_tbl1()),
        server = TRUE,
        selection = 'single'
      )

      dt_row1 <- shiny::reactive({
        shiny::req(dt_tbl1(), input$dt1_rows_selected)
        dplyr::slice(dt_tbl1(), input$dt1_rows_selected)
      })

      filtered_id_tbl1 <- shiny::reactive({
        shiny::req(id_tbl(), dt_row1(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")

        id_tbl() %>%
          dplyr::inner_join(
            dt_row1(),
            by = c(config$milestone_column, config$date_estimate_column)
          ) %>%
          dplyr::select(
            config$format_column,
            config$milestone_column,
            config$expected_files_column
          )
      })

      filtered_files_tbl1 <- shiny::reactive({
        shiny::req(files_tbl(), dt_row1(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")

        date_column <- rlang::sym(config$date_created_column)
        format_column <- rlang::sym(config$format_column)
        actual_column <- rlang::sym(config$actual_files_column)

        files_tbl() %>%
          dplyr::filter(
            !!date_column < dt_row1()[["Date Range End"]],
            !!date_column > dt_row1()[["Date Range Start"]]
          ) %>%
          dplyr::group_by(!!format_column) %>%
          dplyr::summarise(!!actual_column := dplyr::n())
      })

      merged_tbl1 <- shiny::reactive({

        shiny::req(filtered_id_tbl1(), filtered_files_tbl1(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")

        date_column     <- rlang::sym(config$date_created_column)
        actual_column   <- rlang::sym(config$actual_files_column)
        format_column   <- rlang::sym(config$format_column)
        expected_column <- rlang::sym(config$expected_files_column)

        filtered_id_tbl1() %>%
          dplyr::full_join(filtered_files_tbl1(), by = config$format_column) %>%
          dplyr::mutate(
            !!actual_column := dplyr::if_else(
              is.na(!!actual_column),
              0L,
              !!actual_column
            )
          ) %>%
          dplyr::select(!!format_column, !!expected_column, !!actual_column) %>%
          tidyr::pivot_longer(
            cols = -c(!!format_column),
            names_to = "Types of Files",
            values_to = "Number of Files"
          ) %>%
          dplyr::mutate(
            "Types of Files" = base::factor(
              .data$`Types of Files`,
              levels = c(
                config$expected_files_column, config$actual_files_column
              )
            )
          )
      })

      plot_obj1 <- shiny::reactive({
        shiny::req(filtered_id_tbl1(), filtered_files_tbl1(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")
        format_column   <- rlang::sym(config$format_column)

        p <- merged_tbl1() %>%
          ggplot2::ggplot() +
          ggplot2::geom_bar(
            ggplot2::aes(
              x = !!rlang::sym("Types of Files"),
              y = !!rlang::sym("Number of Files"),
              fill = !!rlang::sym("Types of Files")
            ),
            stat = "identity",
            alpha = 0.8,
            na.rm = TRUE,
            show.legend = FALSE,
            position = ggplot2::position_dodge()
          ) +
          sagethemes::theme_sage() +
          ggplot2::facet_grid(cols = ggplot2::vars(!!rlang::sym(format_column))) +
          ggplot2::theme(
            legend.text = ggplot2::element_text(size = 8),
            axis.text.x  = ggplot2::element_blank(),
            text = ggplot2::element_text(size = 10),
            strip.text.x = ggplot2::element_text(size = 10),
            legend.position = "right",
            panel.grid.major.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "grey95")
          )
      })

      output$plot1 <- plotly::renderPlotly({
        shiny::req(plot_obj1())

        plotly::ggplotly(plot_obj1()) %>%
          plotly::layout(
            autosize = T
          )
      })



      # plot2 ----

      milestone_choices <- shiny::reactive({
        shiny::req(id_tbl(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")
        milestone_column   <- rlang::sym(config$milestone_column)

        id_tbl() %>%
          dplyr::pull(!!milestone_column) %>%
          unique() %>%
          sort()
      })

      output$milestone_choice_ui <- shiny::renderUI({
        shiny::req(milestone_choices())
        shiny::selectInput(
          inputId = ns("milestone_choice"),
          label = "Choose Milestone",
          choices = milestone_choices()
        )
      })

      filtered_id_tbl2 <- shiny::reactive({
        shiny::req(id_tbl(), input$milestone_choice, config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")
        milestone_column <- rlang::sym(config$milestone_column)
        expected_column  <- rlang::sym(config$expected_files_column)
        format_column    <- rlang::sym(config$format_column)

        id_tbl() %>%
          dplyr::filter(!!milestone_column == input$milestone_choice) %>%
          dplyr::select(
            !!format_column,
            !!milestone_column,
            !!expected_column
          )
      })

      plot_obj2a <- shiny::reactive({
        shiny::req(filtered_id_tbl2(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")

        expected_column  <- rlang::sym(config$expected_files_column)
        format_column    <- rlang::sym(config$format_column)

        p <- filtered_id_tbl2() %>%
          ggplot2::ggplot() +
          ggplot2::geom_bar(
            ggplot2::aes(
              x = !!format_column,
              y = !!expected_column,
              fill = !!format_column
            ),
            stat = "identity",
            alpha = 0.8,
            na.rm = TRUE,
            show.legend = FALSE,
            position = ggplot2::position_dodge()
          ) +
          sagethemes::theme_sage() +
          ggplot2::theme(
            legend.text = ggplot2::element_text(size = 8),
            axis.text.x  = ggplot2::element_blank(),
            text = ggplot2::element_text(size = 10),
            strip.text.x = ggplot2::element_text(size = 10),
            legend.position = "right",
            panel.grid.major.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "grey95")
          ) +
          ggplot2::labs(
            x = "Files expected for this milestone",
            y = "Number of files"
          )
      })

      output$plot2a <- plotly::renderPlotly({
        shiny::req(plot_obj2a())

        plotly::ggplotly(plot_obj2a()) %>%
          plotly::layout(
            autosize = T
          )
      })

      filtered_files_tbl2 <- shiny::reactive({
        shiny::req(files_tbl(), input$milestone_choice, config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")

        format_column <- rlang::sym(config$format_column)
        actual_column <- rlang::sym(config$actual_files_column)
        milestone_column <- rlang::sym(config$milestone_column)

        files_tbl() %>%
          dplyr::filter(!!milestone_column == input$milestone_choice) %>%
          dplyr::group_by(!!format_column) %>%
          dplyr::summarise(!!actual_column := dplyr::n())
      })


      plot_obj2b <- shiny::reactive({
        shiny::req(filtered_files_tbl2(), config())
        config <- purrr::pluck(config(), "milestone_reporting_plot")

        format_column    <- rlang::sym(config$format_column)
        actual_column    <- rlang::sym(config$actual_files_column)
        milestone_column <- rlang::sym(config$milestone_column)

        p <- filtered_files_tbl2() %>%
          ggplot2::ggplot() +
          ggplot2::geom_bar(
            ggplot2::aes(
              x = !!format_column,
              y = !!actual_column,
              fill = !!format_column
            ),
            stat = "identity",
            alpha = 0.8,
            na.rm = TRUE,
            show.legend = FALSE,
            position = ggplot2::position_dodge()
          ) +
          sagethemes::theme_sage() +
          ggplot2::theme(
            legend.text = ggplot2::element_text(size = 8),
            axis.text.x  = ggplot2::element_blank(),
            text = ggplot2::element_text(size = 10),
            strip.text.x = ggplot2::element_text(size = 10),
            legend.position = "right",
            panel.grid.major.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "grey95")
          ) +
          ggplot2::labs(
            x = "Files annotated with this milestone",
            y = "Number of files"
          )
      })

      output$plot2b <- plotly::renderPlotly({
        shiny::req(plot_obj2b())

        plotly::ggplotly(plot_obj2b()) %>%
          plotly::layout(
            autosize = T
          )
      })

    }
  )
}
