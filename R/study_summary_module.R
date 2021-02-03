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
          merge_studies_module_ui(ns("merge_studies")),
          shinydashboard::box(
            shiny::htmlOutput(ns('study_summary')),
            title = "Study Summary",
            status = "primary",
            solidHeader = T,
            width = 12,
            collapsible = FALSE
          ),
          shinydashboard::box(
            plotly::plotlyOutput(ns('study_timeline_plot')),
            title = "Study Timeline",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE
          ),
          shinydashboard::box(
            plotly::plotlyOutput(ns('data_focus_plot')),
            title = "Data Focus",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE
          ),
          shinydashboard::box(
            plotly::plotlyOutput(ns('annotation_activity_plot')),
            title = "Annotation Activity",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE
          ),
          shinydashboard::box(
            plotly::plotlyOutput(ns('publication_status_plot')),
            title = "Publication Status",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE
          )
        )
      )
    )
  )
}

# Study Summary Module Server

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

      data2 <- merge_studies_module_server("merge_studies", data, config)

      # data2 <- shiny::reactive({
      #   shiny::req(data(), config())
      #   data <- data()
      #   table <- create_merged_table_with_config(
      #     data()$tables,
      #     purrr::pluck(config(), "merged_table")
      #   )
      #   data$tables$merged <- table
      #   return(data)
      # })
      #
      # study_table <- shiny::reactive({
      #
      #   shiny::req(data2(), config())
      #
      #   config <- purrr::pluck(config(), "study_table")
      #
      #   data2() %>%
      #     purrr::pluck("tables", "merged") %>%
      #     dplyr::select_at(
      #       unlist(c(config$group_columns, config$count_columns))
      #     ) %>%
      #     dplyr::group_by_at(unlist(config$group_columns))%>%
      #     dplyr::summarise_at(
      #       unlist(config$count_columns),
      #       dplyr::n_distinct,
      #       na.rm = T
      #     ) %>%
      #     dplyr::ungroup() %>%
      #     format_plot_data_with_config(config) %>%
      #     dplyr::arrange(!!rlang::sym(config$id_column))
      # })
      #
      #
      #
      # output$study_table <- DT::renderDataTable(
      #   base::as.data.frame(study_table()),
      #   server = TRUE,
      #   selection = 'single'
      # )
      #
      # selected_study_name <- shiny::reactive({
      #   shiny::req(
      #     study_table(),
      #     config(),
      #     !is.null(input$study_table_rows_selected)
      #   )
      #
      #   column_name <- purrr::pluck(config(), "study_table", "id_column")
      #
      #   study_table() %>%
      #     dplyr::slice(input$study_table_rows_selected) %>%
      #     dplyr::pull(column_name)
      # })
      #
      # output$study <- shinydashboard::renderInfoBox({
      #   shiny::req(selected_study_name())
      #   shinydashboard::infoBox(
      #     "You have selected",
      #     selected_study_name(),
      #     icon = shiny::icon("file"),
      #     color = "light-blue",
      #     fill = FALSE
      #   )
      # })
      #
      # filtered_merged_table <- shiny::reactive({
      #   column <- purrr::pluck(config(), "merged_table", "filter_column")
      #   shiny::req(data2(), selected_study_name())
      #   filter_list_column(
      #     data2()$tables$merged,
      #     column,
      #     selected_study_name()
      #   )
      # })

      # use filtered merged table/selected study

      output$study_summary <- shiny::renderText({

        shiny::req(data2(), config())

        config <- purrr::pluck(config(), "study_summary")

        data2() %>%
          purrr::pluck("tables", "merged") %>%
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

      output$study_timeline_plot <- plotly::renderPlotly({
        shiny::req(data2(), config())

        config <- purrr::pluck(config(), "study_timeline_plot")

        data <- data2() %>%
          purrr::pluck("tables", "merged") %>%
          format_plot_data_with_config(config) %>%
          tidyr::drop_na()

        validate(need(nrow(data) > 0 , config$empty_table_message))

        create_plot_with_config(
          data, config, "create_study_timeline_plot"
        )
      })

      output$data_focus_plot <- plotly::renderPlotly({

        shiny::req(data2(), config())

        config <- purrr::pluck(config(), "data_focus_plot")

        data_list <- data2() %>%
          purrr::pluck("tables", "merged") %>%
          format_plot_data_with_config(config) %>%
          create_data_focus_tables(config$plot$x, config$plot$fill)

        validate(need(length(data_list) > 0 , config$empty_table_message))

        create_data_focus_plots(data_list, config)
      })

      output$annotation_activity_plot <- plotly::renderPlotly({
        shiny::req(data2(), config())
        config <- purrr::pluck(config(), "annotation_activity_plot")

        data <- data2() %>%
          purrr::pluck("tables", "merged") %>%
          format_plot_data_with_config(config) %>%
          create_plot_count_df(
            factor_columns   = config$plot$x,
            complete_columns = c(config$plot$x, config$plot$facet)
          )

        validate(need(sum(data$Count) > 0, config$empty_table_message))

        create_plot_with_config(
          data, config, "create_annotation_activity_plot"
        )
      })

      output$publication_status_plot <- plotly::renderPlotly({

        shiny::req(config(), data(), data2())

        config <- purrr::pluck(config(), "publication_status_plot")

        data <- data() %>%
          purrr::pluck("tables", config$table) %>%
          filter_list_column(config$filter_column, data2()$selected_study) %>%
          format_plot_data_with_config(config)

        validate(need(nrow(data) > 0 , config$empty_table_message))

        create_plot_with_config(
          data,
          config,
          "create_publication_status_plot"
        )%>%
          plotly::layout(yaxis = list(range = c(0, 5)), autosize = T)
      })

    }
  )
}
