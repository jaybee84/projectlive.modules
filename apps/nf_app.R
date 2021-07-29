devtools::load_all()

ui <- function(req) {
  shiny::tagList(
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = shiny::span(
      style = "color:white;",
      waiter::spin_pulsar(),
      shiny::h3("logging in...")
    )),
    shiny::navbarPage(
      title = shiny::strong("projectLive"), selected = "Data",
      shiny::tabPanel(
        "Data",
        synapse_module_ui2("synapse_module"),
        icon = shiny::icon("chart-area")
      ),
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
      shiny::tabPanel(
        "New Submissions",
        new_submissions_module_ui("new_submissions_module"),
        icon = shiny::icon("chart-area")
      ),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive")
  )
}

server <- function(input, output, session) {

  synapseclient <- reticulate::import("synapseclient", delay_load = TRUE)
  syn <- synapseclient$Synapse()
  syn$login()

  data <- synapse_module_server2(
    id = "synapse_module",
    syn = syn,
    config = shiny::reactive(get_nf_synapse_config())
  )

  summary_snapshot_module_server(
    id = "summary_snapshot_module",
    data = data,
    config = shiny::reactive(get_nf_summary_snapshot_config())
  )

  publication_status_module_server(
    id = "publication_status_module",
    data = data,
    config = shiny::reactive(get_nf_publication_status_config())
  )

  study_summary_module_server(
    id = "study_summary_module",
    data = data,
    config = shiny::reactive(get_nf_study_summary_config())
  )

  new_submissions_module_server(
    id = "new_submissions_module",
    data = data,
    config = shiny::reactive(get_nf_new_submissions_config())
  )
}

shiny::shinyApp(ui, server)




