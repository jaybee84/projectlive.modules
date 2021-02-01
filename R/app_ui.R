ui <- function() {
  shiny::tagList(
    shiny::navbarPage(
      title = shiny::strong("projectLive"), selected = "About",
      shiny::tabPanel(
        "Snapshot",
        summary_snapshot_module_ui("summary_snapshot_module"),
        icon = shiny::icon("chart-area")),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive")
  )
}


