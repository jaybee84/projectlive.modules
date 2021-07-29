# ui ----

#' OAUth UI
#'
#' @param request Request Internal parameter for Shiny
#' @param ui_function A function that returns UI elements
#' @param oauth_list A named list. The list must have the following items:
#' - "endpoint" an object of type httr::oauth_endpoint()
#' - "app" an object pf type httr::oauth_app()
#' - "scope" a character vector
#'
#' @export
oauth_ui <- function(request, ui_function, oauth_list) {
  if (!has_auth_code(shiny::parseQueryString(request$QUERY_STRING))) {
    return(create_oauth_url_script_html(oauth_list))
  } else {
    return(ui_function())
  }
}

#' Create OAuth URL Script HTML
#'
#' @param oauth_list A named list. The list must have the following items:
#' - "endpoint" an object of type httr::oauth_endpoint()
#' - "app" an object pf type httr::oauth_app()
#' - "scope" a character vector
#'
#' @export
create_oauth_url_script_html <- function(oauth_list){

  authorization_url = httr::oauth2.0_authorize_url(
    endpoint = oauth_list$endpoint,
    app      = oauth_list$app,
    scope    = oauth_list$scope
  )

  script <- shiny::tags$script(shiny::HTML(sprintf(
    "location.replace(\"%s\");", authorization_url
  )))
}

# server ----

#' Get OAuth Access Token
#'
#' @param oauth_list A named list. The list must have the following items:
#' - "config" a named list of parameters for theis specific shiny app. It
#' must have a field named "app_url".
#' - "endpoint" an object of type httr::oauth_endpoint()
#' - "app" an object pf type httr::oauth_app()
#' @param session a shiny session
#'
#' @export
get_oauth_access_token <- function(oauth_list, session){

  url_parameters <- shiny::parseQueryString(
    shiny::isolate(session$clientData$url_search)
  )

  if (!has_auth_code(url_parameters)) {
    return()
  }

  redirect_url <- paste0(
    oauth_list$endpoint$access,
    '?',
    'redirect_uri=',
    oauth_list$config$app_url,
    '&grant_type=',
    'authorization_code',
    '&code=',
    url_parameters$code
  )

  # get the access_token and userinfo token
  req <- httr::POST(
    redirect_url,
    encode = "form",
    body = '',
    httr::authenticate(oauth_list$app$key, oauth_list$app$secret, type = "basic"),
    config = list()
  )

  # Stop the code if anything other than 2XX status code is returned
  httr::stop_for_status(req, task = "get an access token")
  token_response <- httr::content(req, type = NULL)
  access_token <- token_response$access_token
}

#' Get OAuth Request With Token
#'
#' @param access_token An OAuth access token
#'
#' @export
get_oauth_request_with_token <- function(access_token){
  response <- httr::content(httr::GET(
    "https://repo-prod.prod.sagebase.org/auth/v1/oauth2/userinfo",
    httr::add_headers(Authorization = glue::glue("Bearer {access_token}"))
  ))
}

# global ----

#' Create OAuth List
#'
#' @param config_file A string, the path to a yaml file
#' @param config  The list must have the following items:
#' - "client_id" An integer or a string containing na integer ie. "1"
#' - "client_secret" A string
#' - "app_url" A string, the shiny app's URL
#' @param scope a character vector
#' @param claims_list A named list
#'
#' @export
create_oauth_list <- function(
  config_file = "inst/oauth_config.yaml",
  config      = NULL,
  scope       = "openid view download modify",
  claims_list = list("userid" = NULL)
){
  if(is.null(config)){
    config   <- get_oauth_config(config_file)
  }
  result <- list(
    "config"   = config,
    "app"      = create_oauth_app(config),
    "endpoint" = create_oauth_endpoint(claims_list),
    "scope"    = scope
  )
}

get_oauth_config <- function(config_file = "inst/oauth_config.yaml"){
  oauth_config = yaml::yaml.load_file(config_file)

  client_id     <- toString(oauth_config$client_id)
  client_secret <- oauth_config$client_secret
  app_url       <- oauth_config$app_url

  if (is.null(client_id)) stop("config.yaml is missing client_id")
  if (is.null(client_secret)) stop("config.yaml is missing client_secret")
  if (is.null(app_url)) stop("config.yaml is missing app_url")

  return(list(
    "client_id" = client_id,
    "client_secret" = client_secret,
    "app_url" = app_url
  ))
}


has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}


#' Get Extended Synapse Claims List
#' @export
get_extended_synapse_claims_list <- function(){
  # These are the user info details ('claims') requested from Synapse:
  return(
    list(
      family_name=NULL,
      given_name=NULL,
      email=NULL,
      email_verified=NULL,
      userid=NULL,
      orcid=NULL,
      is_certified=NULL,
      is_validated=NULL,
      validated_given_name=NULL,
      validated_family_name=NULL,
      validated_location=NULL,
      validated_email=NULL,
      validated_company=NULL,
      validated_at=NULL,
      validated_orcid=NULL,
      company=NULL
    )
  )
}

get_synapse_claims_json <- function(claims_list){
  jsonlite::toJSON(list(id_token = claims_list,  userinfo = claims_list))
}

create_oauth_app <- function(oauth_config){
  httr::oauth_app(
    "shinysynapse",
    key = oauth_config$client_id,
    secret = oauth_config$client_secret,
    redirect_uri = oauth_config$app_url
  )
}

create_oauth_endpoint <- function(claims_list){
  httr::oauth_endpoint(
    authorize = stringr::str_c(
      "https://signin.synapse.org?claims=",
      get_synapse_claims_json(claims_list)
    ),
    access = "https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
  )
}

