csbc_server <- function(input, output, session) {

  data <- shiny::reactive(get_csbc_data())

  summary_snapshot_module_server(
    id = "summary_snapshot_module",
    data = data,
    config = shiny::reactive(get_csbc_summary_snapshot_config())
  )

  publication_status_module_server(
    id = "publication_status_module",
    data = data,
    config = shiny::reactive(get_csbc_publication_status_config())
  )

  study_summary_module_server(
    id = "study_summary_module",
    data = data,
    config = shiny::reactive(get_csbc_study_summary_config())
  )
}
