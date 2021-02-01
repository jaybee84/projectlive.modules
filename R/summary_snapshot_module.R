# Summary Snapshot Module UI
#' @title summary_snapshot_module_server and summary_snapshot_module_server_ui
#' @rdname summary_snapshot_module
#' @description  A shiny Module.
#'
#' @param id shiny id
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
          shinydashboard::box(
            title = "Initiative Activity",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns("initiative_activity"))
          ),
          shinydashboard::box(
            title = "Resources Generated",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns("resources_generated"))
          ),
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

      output$header_text <- shiny::renderText({
        shiny::req(config())
        glue::glue(config()$header_text)
      })

      # plot boxes ----
      output$box1 <- shinydashboard::renderInfoBox({
        shiny::req(config(), data())
        config <- purrr::pluck(
          config(),
          "overview_boxes",
          "box1"
        )
        create_info_box(config, data())
      })

      output$box2 <- shinydashboard::renderInfoBox({
        shiny::req(config(), data())
        config <- purrr::pluck(
          config(),
          "overview_boxes",
          "box2"
        )
        create_info_box(config, data())
      })

      output$box3 <- shinydashboard::renderInfoBox({
        shiny::req(config(), data())
        config <- purrr::pluck(
          config(),
          "overview_boxes",
          "box3"
        )
        create_info_box(config, data())
      })

      output$box4 <- shinydashboard::renderInfoBox({
        shiny::req(config(), data())
        config <- purrr::pluck(
          config(),
          "overview_boxes",
          "box4"
        )
        create_info_box(config, data())
      })

      # initiative_activity ----
      initiative_activity_data <- shiny::reactive({
        shiny::req(config(), data())
        config <- purrr::pluck(config(), "initiative_activity")

        data <- data() %>%
          purrr::pluck("tables", config$table) %>%
          format_plot_data_with_config(config)

        shiny::validate(shiny::need(
          nrow(data) > 0,
          config$empty_table_message
        ))

        return(data)
      })

      output$initiative_activity <- plotly::renderPlotly({
        shiny::req(config(), initiative_activity_data())
        create_plot_with_config(
          data = initiative_activity_data(),
          config = config()$initiative_activity,
          plot_func = "create_initiative_activity_plot"
        )
      })

      # resources_generated ----
      resources_generated_data <- shiny::reactive({
        shiny::req(config(), data())
        config <- purrr::pluck(config(), "resources_generated")

        data <- data() %>%
          purrr::pluck("tables", config$table) %>%
          format_plot_data_with_config(config)

        shiny::validate(shiny::need(nrow(data) > 0, config$empty_table_message))

        return(data)
      })

      output$resources_generated <- plotly::renderPlotly({
        shiny::req(config(), resources_generated_data())
        create_plot_with_config(
          data = resources_generated_data(),
          config = config()$resources_generated,
          plot_func = "create_resources_generated_plot"
        )
      })

      # file_upload_timeline ----
      file_upload_timeline_filter_choices <- shiny::reactive({
        shiny::req(config(), data())

        config <- config()$file_upload_timeline
        column <- config$filter_column

        choices <- data() %>%
          purrr::pluck("tables", config$table) %>%
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
        config <- config()$file_upload_timeline
        data <- data()$tables[[config$table]]

        if (input$file_upload_timeline_filter_value != "All"){
          data <- data %>%
            filter_list_column(
              config$filter_column,
              input$file_upload_timeline_filter_value
            )
        }

        data <- data %>%
          format_plot_data_with_config(config) %>%
          dplyr::mutate("Study Name" = stringr::str_trunc(.data$`Study Name`, 40)) %>%
          create_plot_count_df(
            factor_columns   = c(config$plot$x),
            complete_columns = c(config$plot$x, config$plot$facet)
          )

        shiny::validate(shiny::need(
          sum(data$Count) > 0,
          config$empty_table_message
        ))

        return(data)
      })

      output$file_upload_timeline <- plotly::renderPlotly({

        shiny::req(file_upload_timeline_data(), config())
        config <- config()$file_upload_timeline

        create_plot_with_config(
          data = file_upload_timeline_data(),
          config = config()$file_upload_timeline,
          plot_func =  "create_file_upload_timeline_plot",
          height = 870
        ) %>%
          plotly::layout(
            autosize = T
          )
      })

    }
  )
}


