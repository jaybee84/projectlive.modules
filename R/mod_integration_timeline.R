mod_integration_timeline_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::textOutput(ns("text"))
}

mod_integration_timeline_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      id_tbl <- shiny::reactive({
        data()$tables$incoming_data %>%
          dplyr::filter(
            !is.na(reportMilestone),
            !is.na(date_uploadestimate)
          ) %>%
          dplyr::select(
            "Format" = "fileFormat",
            "Date Estimate" = "date_uploadestimate",
            "Milestone" = "reportMilestone",
            "Expected" = "estimatedMinNumSamples"
          ) %>%
          tidyr::unnest("Format") %>%
          dplyr::group_by(Format, Milestone, `Date Estimate`) %>%
          dplyr::summarise("Expected" = sum(Expected)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            "Expected" = dplyr::if_else(
              is.na(Expected),
              0,
              Expected
            )
          )
      })

      dt_tbl <- shiny::reactive({
        id_tbl() %>%
          dplyr::select(Study, Milestone, `Date Estimate`) %>%
          dplyr::distinct()
      })

      output$dt <- DT::renderDataTable(
        base::as.data.frame(dt_tbl()),
        server = TRUE,
        selection = 'single'
      )

      dt_row <- shiny::reactive({
        dt_tbl() %>%
          dplyr::slice(input$dt_rows_selected)
      })

      filtered_id_tbl <- shiny::reactive({
        id_tbl() %>%
          dplyr::inner_join(dt_row()) %>%
          dplyr::mutate(
            "min_date" = `Date Estimate` - lubridate::duration(100, 'days'),
            "max_date" = `Date Estimate` + lubridate::duration(100, 'days')
          ) %>%
          dplyr::select(-"Date Estimate")
      })

      files <- shiny::reactive({
        files <-
          data()$tables$files %>%
          dplyr::select(
            "Study" = "studyName",
            "Format" = "fileFormat",
            "Date Created" = "date"
          ) %>%
          tidyr::drop_na()
      })

      merged_table <- shiny::reactive({
        x <-
          dplyr::inner_join(filtered_id_tbl(), files(), by = c("Study", "Format")) %>%
          dplyr::filter(
            `Date Created` < max_date,
            `Date Created` > min_date
          ) %>%
          dplyr::group_by(Study, Format, Expected) %>%
          dplyr::summarise("Actual" = dplyr::n())

      })


    }
  )
}
