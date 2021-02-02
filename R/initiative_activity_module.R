# Initiative Activity Module UI
#' @title initiative_activity_module_server and
#' initiative_activity_module_server_ui
#' @rdname initiative_activity_module
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @export
initiative_activity_module_ui <- function(id){
  ns <- shiny::NS(id)
  shinydashboard::box(
    title = "Initiative Activity",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = FALSE,
    plotly::plotlyOutput(ns("plot"))
  )
}

# Initiative Activity Module Server

#' @title initiative_activity_module_server and initiative_activity_module_server_ui
#' @param id shiny id
#' @param data A named list. The list must contain a list named "tables".
#' @param config A named list.
#'
#' @rdname initiative_activity_module
#' @export
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
initiative_activity_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      plot_data <- shiny::reactive({
        shiny::req(data(), config())

        data <- data() %>%
          purrr::pluck("tables", config()$table) %>%
          format_plot_data_with_config(config())

        shiny::validate(shiny::need(
          nrow(data) > 0,
          config()$empty_table_message
        ))

        return(data)
      })

      output$plot <- plotly::renderPlotly({
        shiny::req(config(), plot_data())
        create_plot_with_config(
          data = plot_data(),
          config = config(),
          plot_func = "create_initiative_activity_plot"
        )
      })
    }
  )
}
