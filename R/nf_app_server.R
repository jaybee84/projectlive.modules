nf_server <- function(input, output, session) {

  data <- shiny::reactive(nf_example_data())

  summary_snapshot_module_server(
    id = "summary_snapshot_module",
    data = data,
    config = shiny::reactive(nf_example_summary_snapshot_config())
  )

  publication_status_module_server(
    id = "publication_status_module",
    data = data,
    config = shiny::reactive(nf_example_publication_status_config())
  )

  study_summary_module_server(
    id = "study_summary_module",
    data = data,
    config = shiny::reactive(nf_example_study_summary_config())
  )
}
