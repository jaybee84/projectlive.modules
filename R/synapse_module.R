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
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('group'))
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
#' @param data_config A named list
#'
#' @export
synapse_module_server <- function(id, syn, data_config){
  shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      output$about <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          " ",
          icon = shiny::icon("university", "fa-1x"),
          color = "light-blue",
          fill = TRUE
        )
      })

      output$group <- shiny::renderText({
        txt <- stringr::str_c(
          "Navigate to the tabs at the top of the page to get more information ",
          "about the participating investigators and the various resources ",
          "that they have generated."
        )
        waiter::waiter_hide()
        txt
      })

      data <- shiny::reactive({
        shiny::req(syn, data_config)
        tables <- data_config %>%
          purrr::pluck("data_files") %>%
          purrr::map_chr("synapse_id") %>%
          purrr::map(read_rds_file_from_synapse, syn)
        list(
          "tables" = tables
        )
      })

    }
  )
}
