mod_integration_timeline_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::textOutput(ns("text"))
}

mod_integration_timeline_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$text <- shiny::renderText("TEST")
    }
  )
}