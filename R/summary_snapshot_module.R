# Summary Snapshot Module UI
#' @title summary_snapshot_module_server and summary_snapshot_module_server_ui
#' @rdname summary_snapshot_module
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @export
summary_snapshot_module_ui <- function(id){
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
            shiny::textOutput(ns('header_text'))
          ),
          shinydashboard::box(
            title = "Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            shiny::fluidRow(
              shinydashboard::infoBoxOutput(ns('box1'), width = 3),
              shinydashboard::infoBoxOutput(ns('box2'), width = 3),
              shinydashboard::infoBoxOutput(ns('box3'), width = 3),
              shinydashboard::infoBoxOutput(ns('box4'), width = 3)
            )
          ),
          plot_module_ui(ns("initiative_activity"), "Initiative Activity"),
          plot_module_ui(ns("resources_generated"), "Resources Generated"),
          file_upload_timeline_module_ui(ns("file_upload_timeline"))
        )
      )
    )
  )
}

# Summary Snapshot Module Server

#' @title summary_snapshot_module_server and summary_snapshot_module_server_ui
#' @param id shiny id
#' @param data A named list. The list must contain a list named "tables".
#' @param config A named list. The list must contain lists for each section:
#' - header_text
#' - overview_boxes
#' - initiative_activity
#' - resources_generated
#' - file_upload_timeline
#'
#' @rdname summary_snapshot_module
#' @export
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
summary_snapshot_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      config_is_valid <- shiny::reactive({
        shiny::req(config())
        valid <- all(
          c(
            "header_text",
            "overview_boxes",
            "initiative_activity",
            "resources_generated",
            "file_upload_timeline"
          ) %in% names(config())
        )

        if(!valid) {
          stop("Config missing needed items")
        } else {
          return(T)
        }

      })

      output$header_text <- shiny::renderText({
        shiny::req(config())
        glue::glue(config()$header_text)
      })

      # plot boxes ----
      output$box1 <- shinydashboard::renderInfoBox({
        shiny::req(config_is_valid(), data())
        config <- purrr::pluck(
          config(),
          "overview_boxes",
          "box1"
        )
        create_info_box(config, data())
      })

      output$box2 <- shinydashboard::renderInfoBox({
        shiny::req(config_is_valid(), data())
        config <- purrr::pluck(
          config(),
          "overview_boxes",
          "box2"
        )
        create_info_box(config, data())
      })

      output$box3 <- shinydashboard::renderInfoBox({
        shiny::req(config_is_valid(), data())
        config <- purrr::pluck(
          config(),
          "overview_boxes",
          "box3"
        )
        create_info_box(config, data())
      })

      output$box4 <- shinydashboard::renderInfoBox({
        shiny::req(config_is_valid(), data())
        config <- purrr::pluck(
          config(),
          "overview_boxes",
          "box4"
        )
        create_info_box(config, data())
      })

      plot_module_server(
        id = "initiative_activity",
        data = data,
        config = shiny::reactive(purrr::pluck(config(), "initiative_activity")),
        plot_func = shiny::reactive("create_initiative_activity_plot")
      )

      plot_module_server(
        id = "resources_generated",
        data = data,
        config = shiny::reactive(purrr::pluck(config(), "resources_generated")),
        plot_func = shiny::reactive("create_resources_generated_plot")
      )

      file_upload_timeline_module_server(
        id = "file_upload_timeline",
        data = data,
        config = shiny::reactive(purrr::pluck(config(), "file_upload_timeline"))
      )

    }
  )
}


