nf_ui <- function() {
  shiny::tagList(
    shiny::navbarPage(
      title = shiny::strong("projectLive"), selected = "About",
      shiny::tabPanel(
        "Snapshot",
        summary_snapshot_module_ui("summary_snapshot_module"),
        icon = shiny::icon("chart-area")
      ),
      shiny::tabPanel(
        "Publications",
        publication_status_module_ui("publication_status_module"),
        icon = shiny::icon("chart-area")
      ),
      shiny::tabPanel(
        "Studies",
        study_summary_module_ui("study_summary_module"),
        icon = shiny::icon("chart-area")
      ),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive")
  )
}


