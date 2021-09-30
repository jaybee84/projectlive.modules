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
      title = "Milestone or Progress Report tracker",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      shiny::h4(stringr::str_c(
        "The following plots track the expected and actual data uploads to this",
        "study associated with each progress report or milestone report.",
        sep = " "
      )),
      shiny::p("Select the visualization type for the plots from the list below"),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::uiOutput(ns("join_column_choice_ui"))
        )
      ),
      # ----
      shinydashboard::box(
        title = "Researcher reported progress/milestone update",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shiny::p(stringr::str_c(
          "Select a milestone or progress report number from the list below.",
          "The plots will show the expected data files for this milestone or progress report,",
          "and the uploaded data files that the researcher reported for this milestone or progress report.",
          sep = " "
        )),
        shiny::fluidRow(
          shiny::column(
            width = 2,
            shiny::uiOutput(ns("milestone_choice_ui"))
          )
        ),
        shiny::textOutput(ns("plot_title2")),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            plotly::plotlyOutput(ns("plot2"))
          )
        )
      ),
      # ----
      shinydashboard::box(
        title = "Sage Internal milestone or progress report tracking",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::p(stringr::str_c(
              "Click on a row in the table below to select the milestone or progress report number of interest.",
              "Then use the slider on the right to determine a time window around the",
              "estimated date of upload to find all files uploaded during this window",
              sep = " "
            ))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            DT::dataTableOutput(ns("dt"))
          ),
          shiny::column(
            width = 6,
            shiny::sliderInput(
              inputId = ns("days_choice"),
              label = "Select Amount of Days",
              step = 1L,
              min = 30L,
              max = 365L,
              value = 60L
            )
          )
        ),
        shiny::textOutput(ns("plot_title1")),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            plotly::plotlyOutput(ns("plot1"))
          )
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

      join_column_choices <- shiny::reactive({
        shiny::req(config())
        choices <- unlist(config()$join_columns)
      })

      output$join_column_choice_ui <- shiny::renderUI({
        shiny::req(join_column_choices())
        shiny::selectInput(
          inputId = ns("join_column_choice"),
          label = "Choose paramater to visualize.",
          choices = join_column_choices()
        )
      })

      files_tbl <- shiny::reactive({
        shiny::req(data(), config())
        shiny::validate(shiny::need(
          !is.null(config()),
          "Not tracking milestones or progress reports"
        ))
        config <- purrr::pluck(config(), "files_table")
        tbl <- purrr::pluck(data(), "tables", config$name)
        format_plot_data_with_config(tbl, config)
      })

      id_tbl <- shiny::reactive({
        shiny::req(data(), config())
        shiny::validate(shiny::need(
          !is.null(config()),
          "Not tracking milestones or progress reports"
        ))
        config <- purrr::pluck(config(), "incoming_data_table")
        tbl <- purrr::pluck(data(), "tables", config$name)
        shiny::validate(shiny::need(
          nrow(tbl) > 0,
          "Study has no current milestones or progress report updates."
        ))
        format_plot_data_with_config(tbl, config)
      })

      # plot1 ----

      # output$value_box_1 <- shinydashboard::renderValueBox(
      #   shinydashboard::valueBox(
      #     value = "Sage Internal milestone tracking",
      #     subtitle = NULL,
      #     width = NULL,
      #     color = "blue"
      #   )
      # )

      dt_tbl <- shiny::reactive({
        shiny::req(id_tbl(), config())
        config <- config()
        create_internal_tracking_datatable(id_tbl(), config())
      })

      output$dt <- DT::renderDataTable(
        base::as.data.frame(dt_tbl()),
        server = TRUE,
        selection = list(mode = 'single', selected = 1),
        rownames = FALSE
      )

      dt_row <- shiny::reactive({
        shiny::req(dt_tbl(), input$dt_rows_selected)
        dplyr::slice(dt_tbl(), input$dt_rows_selected)
      })

      selected_milestone <- shiny::reactive({
        shiny::req(dt_row())
        milestone_column <- rlang::sym(config()$milestone_column)
        milestone <- dplyr::pull(dt_row(), !!milestone_column)
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

      plot_title1 <- shiny::reactive({
        shiny::req(date_range_start(), date_range_end())
        stringr::str_c(
          "The plot below is showing all files expected or uploaded between the time window of ",
          as.character(date_range_start()),
          ", and ",
          as.character(date_range_end()),
          "."
        )
      })

      output$plot_title1 <- shiny::renderText({
        shiny::req(plot_title1())
        plot_title1()
      })

      filtered_files_tbl1 <- shiny::reactive({
        shiny::req(
          files_tbl(),
          config(),
          date_range_start(),
          date_range_end(),
          input$join_column_choice
        )
        filter_files_tbl(
          files_tbl(),
          config(),
          date_range_start(),
          date_range_end(),
          input$join_column_choice
        )
      })


      filtered_id_tbl1 <- shiny::reactive({
        shiny::req(
          id_tbl(),
          config(),
          selected_milestone(),
          input$join_column_choice
        )
        filter_internal_data_tbl(
          id_tbl(),
          config(),
          selected_milestone(),
          input$join_column_choice
        )
      })


      merged_tbl1 <- shiny::reactive({
        shiny::req(
          filtered_id_tbl1(),
          filtered_files_tbl1(),
          config(),
          input$join_column_choice
        )

        merge_tbls(
          filtered_id_tbl1(),
          filtered_files_tbl1(),
          config(),
          input$join_column_choice
        )
      })

      plot_obj1 <- shiny::reactive({
        shiny::req(
          merged_tbl1(),
          input$join_column_choice
        )
        create_milestone_reporting_plot(
          merged_tbl1(),
          input$join_column_choice
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
          label = "Choose Milestone or Progress Report Number",
          choices = milestone_choices()
        )
      })

      plot_title2 <- shiny::reactive({
        shiny::req(input$milestone_choice)
        stringr::str_c(
          "The plot below is showing all files expected or annotated with milestone or progress report number ",
          input$milestone_choice,
          "."
        )
      })

      output$plot_title2 <- shiny::renderText({
        shiny::req(plot_title2())
        plot_title2()
      })

      filtered_files_tbl2 <- shiny::reactive({
        shiny::req(
          files_tbl(),
          config(),
          input$milestone_choice,
          input$join_column_choice
        )
        filter_files_tbl2(
          files_tbl(),
          config(),
          input$milestone_choice,
          input$join_column_choice
        )
      })

      filtered_id_tbl2 <- shiny::reactive({
        shiny::req(
          id_tbl(),
          config(),
          input$milestone_choice,
          input$join_column_choice
        )
        filter_internal_data_tbl(
          id_tbl(),
          config(),
          input$milestone_choice,
          input$join_column_choice
        )
      })

      merged_tbl2 <- shiny::reactive({

        shiny::req(
          filtered_id_tbl2(),
          filtered_files_tbl2(),
          config(),
          input$join_column_choice
        )

        merge_tbls(
          filtered_id_tbl2(),
          filtered_files_tbl2(),
          config(),
          input$join_column_choice
        )
      })

      plot_obj2 <- shiny::reactive({
        shiny::req(
          merged_tbl2(),
          input$join_column_choice
        )
        create_milestone_reporting_plot(
          merged_tbl2(),
          input$join_column_choice
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

