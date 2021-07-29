
#' Synapse Module UI2
#' @title synapse_module_ui and synapse_module_server
#' @description  A shiny module.
#' @param id A string, shiny id
#' @export
synapse_module_ui2 <- function(id){
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
            shiny::uiOutput(ns("group_selection_ui")),
            shiny::textOutput(ns('group'))
          ),
          shiny::actionButton(
            inputId = 'back_to_portal',
            label = "Back to the NF Data Portal",
            icon = shiny::icon("map-marker-alt"),
            lib = "font-awesome",
            class = "btn btn-primary btn-lg btn-block",
            onclick ="window.open('https://nf.synapse.org/', '_blank')"
          )
        )
      )
    )
  )
}

#' Synapse Module Server2
#' @title synapse_module_ui and synapse_module_server
#' @description  A shiny module.
#'
#' @param id A string, shiny id
#' @param syn A synapseclient$Synapse object that has been logged in
#' @param config a shiny::recative that returns a named list
#'
#' @export
synapse_module_server2 <- function(id, syn, config){
  shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      current_user_synapse_id <- shiny::reactive({
        shiny::req(syn)
        user <- syn$getUserProfile()[['ownerId']]
        return(user)
      })

      output$about <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "projectLive: Track the progress and impact of your funding initiatives in real time",
          icon = shiny::icon("university", "fa-1x"),
          color = "light-blue",
          fill = TRUE
        )
      })

      groups_allowed <- shiny::reactive({
        shiny::req(syn, config(), current_user_synapse_id())
        get_allowed_groups_from_synapse_user_id(
          syn = syn,
          config = config(),
          synapse_user_id = current_user_synapse_id()
        )
      })

      output$group_selection_ui <- shiny::renderUI({
        shiny::req(groups_allowed())
        shiny::selectizeInput(
          ns("selected_group"),
          label = "",
          choices = groups_allowed(),
          multiple = F
        )
      })

      output$group <- shiny::renderText({
        shiny::req(config())
        txt <- glue::glue(config()$group_text)
        waiter::waiter_hide()
        txt
      })

      tables <- shiny::reactive({
        shiny::req(syn, config())
        get_tables_from_synapse(syn, config())
      })

      filtered_tables <- shiny::reactive({
        shiny::req(tables(), config(), input$selected_group)
        filter_table_list(tables(), config(), input$selected_group)
      })

      data <- shiny::reactive({
        shiny::req(filtered_tables(), input$selected_group)
        list(
          "tables" = filtered_tables(),
          "selected_group" = input$selected_group
        )
      })
    }
  )
}
