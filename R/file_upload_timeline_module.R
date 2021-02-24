#' File Upload Timeline Module UI
#'
#' @rdname plot_module
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @export
file_upload_timeline_module_ui <- function(id){
  ns <- shiny::NS(id)
  shinydashboard::box(
    title = "File Upload Timeline",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    height = 1000,
    collapsible = FALSE,
    shiny::uiOutput(ns("file_upload_timeline_filter_ui")),
    plotly::plotlyOutput(ns('file_upload_timeline'))
  )
}

#' File Upload Timeline Server
#'
#' @param id shiny id
#' @param data A shiny::reactive() that returns a named list. The list must
#' contain a list named "tables".
#' @param config A shiny::reactive() that returns a named list.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
file_upload_timeline_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      file_upload_timeline_filter_choices <- shiny::reactive({
        shiny::req(config(), data())

        column <- config()$filter_column

        choices <- data() %>%
          purrr::pluck("tables", config()$table) %>%
          dplyr::pull(column) %>%
          unlist(.) %>%
          unique() %>%
          sort() %>%
          c("All", .)
      })

      output$file_upload_timeline_filter_ui <- shiny::renderUI({
        shiny::req(file_upload_timeline_filter_choices())
        shiny::selectInput(
          inputId = ns("file_upload_timeline_filter_value"),
          label   = "Select an initiative",
          choices = file_upload_timeline_filter_choices()
        )
      })

      file_upload_timeline_data <- shiny::reactive({
        shiny::req(data(), config(), input$file_upload_timeline_filter_value)
        data <- data()$tables[[config()$table]]

        if (input$file_upload_timeline_filter_value != "All"){
          data <- data %>%
            filter_list_column(
              config()$filter_column,
              input$file_upload_timeline_filter_value
            )
        }

        data <- data %>%
          format_plot_data_with_config(config()) %>%
          create_plot_count_df_with_config(config())

        shiny::validate(shiny::need(
          sum(data$Count) > 0,
          config()$empty_table_message
        ))

        return(data)
      })

      output$file_upload_timeline <- plotly::renderPlotly({

        shiny::req(file_upload_timeline_data(), config())

        create_plot_with_config(
          data = file_upload_timeline_data(),
          config = config(),
          plot_func =  "create_file_upload_timeline_plot",
          height = 870
        )
      })
    }
  )
}
