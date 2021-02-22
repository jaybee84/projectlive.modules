#' Publication Status Module UI
#'
#' @title publication_status_module_ui and publication_status_module_server
#' @rdname publication_status_module
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @export
publication_status_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
          shinydashboard::box(
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('header_text')),
          ),
          plot_module_ui(
            ns("publication_status"),
            "Publication Status"
          ),
          plot_module_ui(
            ns("publication_disease"),
            "Publication Status by Disease Manifestation"
          )
        )
      )
    )
  )
}

# Publication Status Module UI

#' @title publication_status_module_ui and publication_status_module_server
#' @rdname publication_status_module
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param data A named list. It must contain a list named "tables".
#' @param config A named list. It contain a list for each section:
#' - publication_status
#' - publication_disease
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data

publication_status_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$header_text <- shiny::renderText({
        shiny::req(config())
        glue::glue(config()$header_text)
      })

      plot_module_server(
        id = "publication_status",
        data = data,
        config = shiny::reactive(purrr::pluck(config(), "publication_status")),
        plot_func = shiny::reactive("create_publication_status_plot")
      )

      plot_module_server(
        id = "publication_disease",
        data = data,
        config = shiny::reactive(purrr::pluck(config(), "publication_disease")),
        plot_func = shiny::reactive("create_publication_disease_plot")
      )

    }
  )
}


