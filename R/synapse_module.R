#' Synapse Module UI
#' @title synapse_module_ui and synapse_module_server
#' @description  A shiny module.
#' @param id A string, shiny id
#' @export
synapse_module_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
          shinydashboard::infoBoxOutput(ns('about'), width = 12),
          shinydashboard::box(
            title = "Instructions",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('txt'))
          )
        )
      )
    )
  )
}

#' Synapse Module Server
#' @title synapse_module_ui and synapse_module_server
#' @description  A shiny module.
#'
#' @param id A string, shiny id
#' @param syn A synapseclient$Synapse object that has been logged in
#' @param config a shiny::recative that returns a named list
#'
#' @export
synapse_module_server <- function(id, syn, config){
  shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      output$about <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "projectLive: Track the progress and impact of your funding initiatives in real time",
          icon = shiny::icon("university", "fa-1x"),
          color = "light-blue",
          fill = TRUE
        )
      })

      output$txt <- shiny::renderText({
        shiny::req(config())
        txt <- glue::glue(config()$text)
        waiter::waiter_hide()
        txt
      })

      tables <- shiny::reactive({
        shiny::req(syn, config())
        get_tables_from_synapse(syn, config())
      })

      data <- shiny::reactive({
        shiny::req(tables())
        list( "tables" = tables())
      })

    }
  )
}
