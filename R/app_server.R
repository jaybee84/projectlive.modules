server <- function(input, output, session) {

  data <- shiny::reactive(example_tables())

  summary_snapshot_module_server(
    id = "summary_snapshot_module",
    data = data,
    config = shiny::reactive(example_summary_snapshot_module_config())
  )
}
