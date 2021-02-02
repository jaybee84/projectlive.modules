server <- function(input, output, session) {

  data <- shiny::reactive(nf_example_data())

  summary_snapshot_module_server(
    id = "summary_snapshot_module",
    data = data,
    config = shiny::reactive(nf_example_summary_snapshot_config())
  )
}
