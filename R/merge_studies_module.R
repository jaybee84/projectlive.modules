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
      DT::dataTableOutput(ns('study_table')),
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

      merged_table <- shiny::reactive({
        shiny::req(data(), config())
        data <- data()
        create_merged_table_with_config(
          data()$tables,
          purrr::pluck(config(), "merged_table")
        )
      })

      study_table <- shiny::reactive({

        shiny::req(merged_table(), config())

        config <- purrr::pluck(config(), "study_table")

        merged_table() %>%
          dplyr::select_at(
            unlist(c(config$group_columns, config$count_columns))
          ) %>%
          dplyr::group_by_at(unlist(config$group_columns))%>%
          dplyr::summarise_at(
            unlist(config$count_columns),
            dplyr::n_distinct,
            na.rm = T
          ) %>%
          dplyr::ungroup() %>%
          format_plot_data_with_config(config) %>%
          dplyr::arrange(!!rlang::sym(config$id_column))
      })

      output$study_table <- DT::renderDataTable(
        base::as.data.frame(study_table()),
        server = TRUE,
        selection = 'single'
      )

      selected_study_name <- shiny::reactive({
        shiny::req(
          study_table(),
          config(),
          !is.null(input$study_table_rows_selected)
        )

        column_name <- purrr::pluck(config(), "study_table", "id_column")

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

      filtered_merged_table <- shiny::reactive({
        column <- purrr::pluck(config(), "merged_table", "filter_column")
        shiny::req(merged_table(), selected_study_name())
        filter_list_column(
          merged_table(),
          column,
          selected_study_name()
        )
      })

      data2 <- shiny::reactive({
        shiny::req(data(), filtered_merged_table(), selected_study_name())
        data <- data()
        data$tables$merged <- filtered_merged_table ()
        data$selected_study <- selected_study_name()
        return(data)
      })

      return(data2)

    }
  )
}

