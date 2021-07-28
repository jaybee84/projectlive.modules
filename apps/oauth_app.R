devtools::load_all(".")

if (interactive()) {
  options(shiny.port = 8100)
}

OAUTH_LIST <- create_oauth_list(
  config_file = "../inst/oauth_config.yaml",
)

ui <- function(req) {
  ui_function <- function(){
    shiny::tagList(
      shiny::uiOutput("title")
    )
  }
  oauth_ui(req, ui_function, OAUTH_LIST)
}

server <- function(input, output, session) {

  access_token = get_oauth_access_token(
    oauth_list = OAUTH_LIST, session = session
  )

  synapseclient <- reticulate::import("synapseclient", delay_load = TRUE)
  syn <- synapseclient$Synapse()
  syn$login(authToken = access_token)

  output$title <- shiny::renderUI({
    shiny::titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
  })

}

shiny::shinyApp(ui, server)




