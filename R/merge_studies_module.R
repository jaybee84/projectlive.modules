# Merge Studies Module UI

#' @title merge_studies_module_ui and merge_studies_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname merge_studies_module
merge_studies_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::box(
      DT::dataTableOutput(ns("study_table")),
      title = "Participating Studies",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE
    ),
    shinydashboard::box(
      shinydashboard::infoBoxOutput(ns('study'), width = 12),
      title = "",
      status = "primary",
      solidHeader = F,
      width = 12,
      collapsible = FALSE,
    )
  )

}

# Merge Studies Module Server

#' @rdname merge_studies_module
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
merge_studies_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      study_table <- shiny::reactive({

        shiny::req(data(), config())

        config <- purrr::pluck(config(), "study_table")

        files <- summarise_df_counts(
          data = purrr::pluck(data(), "tables", "files"),
          group_column = config$join_column,
          columns = config$tables$files$columns
        )

        tools <- summarise_df_counts(
          data = purrr::pluck(data(), "tables", "tools"),
          group_column = config$join_column,
          columns = config$tables$tools$columns
        )

        data() %>%
          purrr::pluck("tables", "studies") %>%
          dplyr::select(dplyr::all_of(
            unlist(config$tables$studies$columns)
          )) %>%
          dplyr::left_join(files, by = config$join_column) %>%
          dplyr::left_join(tools, by = config$join_column) %>%
          format_plot_data_with_config(config$tables$merged)
      })

      output$study_table <- DT::renderDataTable(
        base::as.data.frame(study_table()),
        server = TRUE,
        selection = 'single'
      )

      selected_study_name <- shiny::reactive({
        shiny::req(study_table(),
          config(),
          !is.null(input$study_table_rows_selected)
        )

        column_name <- purrr::pluck(config(), "study_name_column")

        study_table() %>%
          dplyr::slice(input$study_table_rows_selected) %>%
          dplyr::pull(column_name)
      })

      output$study <- shinydashboard::renderInfoBox({
        shiny::req(selected_study_name())
        shinydashboard::infoBox(
          "You have selected",
          selected_study_name(),
          icon = shiny::icon("file"),
          color = "light-blue",
          fill = FALSE
        )
      })

      data2 <- shiny::reactive({
        shiny::req(data(), selected_study_name())
        data <- data()
        data$selected_study <- selected_study_name()
        return(data)
      })

      return(data2)

    }
  )
}

