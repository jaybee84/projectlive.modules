# Milestone Reporting Module UI

#' @title milestone_reporting_module_ui and milestone_reporting_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname milestone_reporting_module
#' @export
milestone_reporting_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::box(
      title = "Internal milestone tracker",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      shiny::fluidRow(
        shiny::column(
          width = 4,
          DT::dataTableOutput(ns("dt"))
        ),
        shiny::column(
          width = 4,
          shiny::numericInput(
            inputId = ns("days_choice"),
            label = "Select Amount of Days",
            value = 60L,
            min = 0L,
            step = 1L
          )
        ),
        shiny::column(width = 4, shiny::textOutput(ns("date_range_string")))
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          plotly::plotlyOutput(ns("plot1"))
        )
      )
    ),
    shinydashboard::box(
      title = "Researcher reported milestone upload",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::uiOutput(ns("milestone_choice_ui"))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          plotly::plotlyOutput(ns("plot2"))
        )
      )
    )
  )

}

# Milestone Reporting Module Server

#' @title milestone_reporting_module_server and milestone_reporting_module_server_ui
#' @param data A named list. The list must contain a list named "tables".
#' @param config A named list.
#' @rdname milestone_reporting_module
#' @export
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
milestone_reporting_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      files_tbl <- shiny::reactive({
        shiny::req(data(), config())
        shiny::validate(shiny::need(
          !is.null(config()),
          "Not Tracking Milestones"
        ))
        config <- purrr::pluck(config(), "files_table")
        tbl <- purrr::pluck(data(), "tables", config$name)
        format_plot_data_with_config(tbl, config)
      })

      id_tbl <- shiny::reactive({
        shiny::req(data(), config())
        shiny::validate(shiny::need(
          !is.null(config()),
          "Not Tracking Milestones"
        ))
        config <- purrr::pluck(config(), "incoming_data_table")
        tbl <- purrr::pluck(data(), "tables", config$name)
        shiny::validate(shiny::need(
          nrow(tbl) > 0,
          "Study has no current milestones."
        ))
        format_plot_data_with_config(tbl, config)
      })

      # plot1 ----

      dt_tbl <- shiny::reactive({
        shiny::req(id_tbl(), config())
        config <- config()
        date_column        <- rlang::sym(config$date_estimate_column)
        milestone_column   <- rlang::sym(config$milestone_column)

        tbl <- id_tbl() %>%
          dplyr::filter(!is.na(!!date_column)) %>%
          dplyr::select(!!milestone_column, !!date_column) %>%
          dplyr::arrange(!!date_column) %>%
          dplyr::distinct()
          # dplyr::mutate(
          #   "Date Range Start" =
          #     !!date_column - lubridate::duration(input$days_choice, 'days'),
          #   "Date Range End" =
          #     !!date_column + lubridate::duration(input$days_choice, 'days')
          # )
      })

      output$dt <- DT::renderDataTable(
        base::as.data.frame(dt_tbl()),
        server = TRUE,
        selection = 'single'
      )

      dt_row <- shiny::reactive({
        shiny::req(dt_tbl(), input$dt_rows_selected)
        dplyr::slice(dt_tbl(), input$dt_rows_selected)
      })

      date_range_start <- shiny::reactive({
        shiny::req(input$days_choice, dt_row(), config())
        date_column <- rlang::sym(config()$date_estimate_column)
        date <- dplyr::pull(dt_row(), !!date_column)
        date - lubridate::duration(input$days_choice, 'days')
      })

      date_range_end <- shiny::reactive({
        shiny::req(input$days_choice, dt_row(), config())
        date_column <- rlang::sym(config()$date_estimate_column)
        date <- dplyr::pull(dt_row(), !!date_column)
        date + lubridate::duration(input$days_choice, 'days')
      })

      date_range_string <- shiny::reactive({
        shiny::req(date_range_start(), date_range_end())
        stringr::str_c(
          "Selecting files with date estimates between ",
          as.character(date_range_start()),
          ", and ",
          as.character(date_range_end()),
          "."
        )
      })

      output$date_range_string  <- shiny::renderText(date_range_string())

      filtered_id_tbl1 <- shiny::reactive({
        shiny::req(id_tbl(), dt_row(), config())
        config <- config()

        id_tbl() %>%
          dplyr::inner_join(
            dt_row(),
            by = c(config$milestone_column, config$date_estimate_column)
          ) %>%
          dplyr::select(
            config$format_column,
            config$milestone_column,
            config$expected_files_column
          )
      })

      filtered_files_tbl1 <- shiny::reactive({
        shiny::req(files_tbl(), config(), date_range_start(), date_range_end())
        config <- config()

        date_column <- rlang::sym(config$date_created_column)
        format_column <- rlang::sym(config$format_column)
        actual_column <- rlang::sym(config$actual_files_column)

        files_tbl() %>%
          dplyr::filter(
            !!date_column < date_range_end(),
            !!date_column > date_range_start()
          ) %>%
          dplyr::group_by(!!format_column) %>%
          dplyr::summarise(!!actual_column := dplyr::n())
      })

      merged_tbl1 <- shiny::reactive({

        shiny::req(filtered_id_tbl1(), filtered_files_tbl1(), config())
        config <- config()

        date_column     <- rlang::sym(config$date_created_column)
        actual_column   <- rlang::sym(config$actual_files_column)
        format_column   <- rlang::sym(config$format_column)
        expected_column <- rlang::sym(config$expected_files_column)

        filtered_id_tbl1() %>%
          dplyr::full_join(filtered_files_tbl1(), by = config$format_column) %>%
          dplyr::mutate(
            !!actual_column := dplyr::if_else(
              is.na(!!actual_column),
              0L,
              !!actual_column
            )
          ) %>%
          dplyr::select(!!format_column, !!expected_column, !!actual_column) %>%
          tidyr::pivot_longer(
            cols = -c(!!format_column),
            names_to = "Types of Files",
            values_to = "Number of Files"
          ) %>%
          dplyr::mutate(
            "Types of Files" = base::factor(
              .data$`Types of Files`,
              levels = c(
                config$expected_files_column, config$actual_files_column
              )
            )
          )
      })

      plot_obj1 <- shiny::reactive({
        shiny::req(merged_tbl1(), config())
        config <- config()
        format_column   <- rlang::sym(config$format_column)

        p <- merged_tbl1() %>%
          ggplot2::ggplot() +
          ggplot2::geom_bar(
            ggplot2::aes(
              x = !!rlang::sym("Types of Files"),
              y = !!rlang::sym("Number of Files"),
              fill = !!rlang::sym("Types of Files")
            ),
            stat = "identity",
            alpha = 0.8,
            na.rm = TRUE,
            show.legend = FALSE,
            position = ggplot2::position_dodge()
          ) +
          sagethemes::theme_sage() +
          ggplot2::facet_grid(cols = ggplot2::vars(!!rlang::sym(format_column))) +
          ggplot2::theme(
            legend.text = ggplot2::element_text(size = 8),
            axis.text.x  = ggplot2::element_blank(),
            text = ggplot2::element_text(size = 10),
            strip.text.x = ggplot2::element_text(size = 10),
            legend.position = "right",
            panel.grid.major.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "grey95")
          )
      })

      output$plot1 <- plotly::renderPlotly({
        shiny::req(plot_obj1())

        plotly::ggplotly(plot_obj1()) %>%
          plotly::layout(
            autosize = T
          )
      })

      # plot2 ----

      milestone_choices <- shiny::reactive({
        shiny::req(id_tbl(), config())
        config <- config()
        milestone_column   <- rlang::sym(config$milestone_column)

        id_tbl() %>%
          dplyr::pull(!!milestone_column) %>%
          unique() %>%
          sort()
      })

      output$milestone_choice_ui <- shiny::renderUI({
        shiny::req(milestone_choices())
        shiny::selectInput(
          inputId = ns("milestone_choice"),
          label = "Choose Milestone",
          choices = milestone_choices()
        )
      })

      filtered_files_tbl2 <- shiny::reactive({
        shiny::req(files_tbl(), input$milestone_choice, config())
        config <- config()

        format_column <- rlang::sym(config$format_column)
        actual_column <- rlang::sym(config$actual_files_column)
        milestone_column <- rlang::sym(config$milestone_column)

        files_tbl() %>%
          dplyr::filter(!!milestone_column == input$milestone_choice) %>%
          dplyr::group_by(!!format_column) %>%
          dplyr::summarise(!!actual_column := dplyr::n())
      })

      filtered_id_tbl2 <- shiny::reactive({
        shiny::req(id_tbl(), input$milestone_choice, config())
        config <- config()
        milestone_column <- rlang::sym(config$milestone_column)
        expected_column  <- rlang::sym(config$expected_files_column)
        format_column    <- rlang::sym(config$format_column)

        id_tbl() %>%
          dplyr::filter(!!milestone_column == input$milestone_choice) %>%
          dplyr::select(
            !!format_column,
            !!milestone_column,
            !!expected_column
          )
      })

      merged_tbl2 <- shiny::reactive({

        shiny::req(filtered_id_tbl2(), filtered_files_tbl2(), config())
        config <- config()

        date_column     <- rlang::sym(config$date_created_column)
        actual_column   <- rlang::sym(config$actual_files_column)
        format_column   <- rlang::sym(config$format_column)
        expected_column <- rlang::sym(config$expected_files_column)

        filtered_id_tbl2() %>%
          dplyr::full_join(filtered_files_tbl2(), by = config$format_column) %>%
          dplyr::mutate(
            !!actual_column := dplyr::if_else(
              is.na(!!actual_column),
              0L,
              !!actual_column
            )
          ) %>%
          dplyr::select(!!format_column, !!expected_column, !!actual_column) %>%
          tidyr::pivot_longer(
            cols = -c(!!format_column),
            names_to = "Types of Files",
            values_to = "Number of Files"
          ) %>%
          dplyr::mutate(
            "Types of Files" = base::factor(
              .data$`Types of Files`,
              levels = c(
                config$expected_files_column, config$actual_files_column
              )
            )
          )
      })

      plot_obj2 <- shiny::reactive({
        shiny::req(merged_tbl2(), config())
        config <- config()
        format_column   <- rlang::sym(config$format_column)

        p <- merged_tbl2() %>%
          ggplot2::ggplot() +
          ggplot2::geom_bar(
            ggplot2::aes(
              x = !!rlang::sym("Types of Files"),
              y = !!rlang::sym("Number of Files"),
              fill = !!rlang::sym("Types of Files")
            ),
            stat = "identity",
            alpha = 0.8,
            na.rm = TRUE,
            show.legend = FALSE,
            position = ggplot2::position_dodge()
          ) +
          sagethemes::theme_sage() +
          ggplot2::facet_grid(cols = ggplot2::vars(!!rlang::sym(format_column))) +
          ggplot2::theme(
            legend.text = ggplot2::element_text(size = 8),
            axis.text.x  = ggplot2::element_blank(),
            text = ggplot2::element_text(size = 10),
            strip.text.x = ggplot2::element_text(size = 10),
            legend.position = "right",
            panel.grid.major.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "grey95")
          )
      })

      output$plot2 <- plotly::renderPlotly({
        shiny::req(plot_obj2())

        plotly::ggplotly(plot_obj2()) %>%
          plotly::layout(
            autosize = T
          )
      })

    }
  )
}

