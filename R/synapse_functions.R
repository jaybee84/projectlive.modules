create_synapse_login <- function(){
  if("use_conda_env.R" %in% list.files("R")) source("R/use_conda_env.R")
  synapseclient <- reticulate::import("synapseclient")
  syn <- synapseclient$Synapse()
  syn$login()
  return(syn)
}

#' Get Synapse Table
#'
#' @param syn An object of reticulate::import("synapseclient")$Synapse()
#' @param table_id A synapse table id
#' @param columns A vector of columns to get from the syanpse table
#' @param limit An integer, limits the number of rows in the table
#' @param ... Argument list to readr::read_csv()
#'
#' @export
get_synapse_tbl <- function(syn, table_id, columns = NULL, limit = NULL, ...){
  list_columns <- table_id %>%
    syn$getTableColumns() %>%
    reticulate::iterate(.) %>%
    purrr::keep(
      .,
      stringr::str_detect(
        purrr::map_chr(., purrr::pluck, "columnType"), "_LIST"
      )
    ) %>%
    purrr::map_chr(purrr::pluck("name"))

  if(is.null(columns)){
    column_string <- "*"
  } else {
    column_string <- stringr::str_c(columns, collapse = ", ")
    list_columns <- purrr::keep(list_columns, list_columns %in% columns)
  }

  query_string <- glue::glue("SELECT {column_string} FROM {table_id}")

  if(is.null(limit)){
    query_string <- glue::glue("SELECT {column_string} FROM {table_id}")
  } else {
    query_string <- glue::glue(
      "SELECT {column_string} FROM {table_id} limit {limit}"
    )
  }
  query_string %>%
    syn$tableQuery(includeRowIdAndRowVersion = F) %>%
    purrr::pluck("filepath") %>%
    readr::read_csv(., ...) %>%
    dplyr::mutate_at(
      list_columns, ~stringr::str_remove_all(.x, '[\\"\\[\\]\\\\]')
    ) %>%
    dplyr::mutate_at(list_columns, ~stringr::str_split(.x, ", "))
}

read_rds_file_from_synapse <- function(synapse_id, syn){
  synapse_id %>%
    syn$get(.) %>%
    purrr::pluck("path") %>%
    readRDS(.)
}

get_acl_principal_ids <- function(syn, entity_id){
  principal_ids <-
    glue::glue("/entity/{entity_id}/acl") %>%
    syn$restGET() %>%
    purrr::pluck("resourceAccess") %>%
    purrr::map_chr("principalId")
}

get_user_teams_ids <- function(syn, user_id){
  team_ids <-
    glue::glue("/user/{user_id}/team?limit=10000") %>%
    syn$restGET() %>%
    purrr::pluck("results") %>%
    purrr::map_chr("id")
}

format_synapse_table_query <- function(
  syn,
  query_string,
  list_columns = NULL,
  ...
){
  tbl <- query_string %>%
    syn$tableQuery(includeRowIdAndRowVersion = F) %>%
    purrr::pluck("filepath") %>%
    readr::read_csv(...)

  if(is.null(list_columns)){
    list_columns <- names(tbl)
  }

  tbl %>%
    dplyr::mutate_at(
      list_columns, ~stringr::str_remove_all(.x, '[\\"\\[\\]\\\\]')
    ) %>%
    dplyr::mutate_at(list_columns, ~stringr::str_split(.x, ", "))
}

# synapse module functions ----

get_allowed_groups_from_synapse_user_id <- function(syn, config, synapse_user_id){
  user_teams    <- get_user_teams_ids(syn, synapse_user_id)
  allowed_teams <- get_acl_principal_ids(syn, config$acl_synapse_id)
  allowed_user_teams <- user_teams[user_teams %in% allowed_teams]

  create_team_table_from_synapse(syn, config) %>%
    dplyr::filter(.data$teams %in% allowed_user_teams) %>%
    dplyr::pull("groups") %>%
    unlist() %>%
    unique() %>%
    sort()
}

create_team_table_from_synapse <- function(syn, config){
  synapse_id <- purrr::pluck(config, "team_table", "synapse_id")
  team_col <- purrr::pluck(config, "team_table", "team_column")
  group_col <- purrr::pluck(config, "team_table", "group_column")

  tbl <-
    glue::glue(
      "SELECT '{team_col}' AS t, '{group_col}' AS g FROM {synapse_id}"
    ) %>%
    format_synapse_table_query(syn, query_string = .) %>%
    dplyr::select("teams" = "t", "groups" = "g")
}

get_tables_from_synapse <- function(syn, config){
  config %>%
    purrr::pluck("data_files") %>%
    purrr::map_chr("synapse_id") %>%
    purrr::map(read_rds_file_from_synapse, syn)
}

filter_table_list <- function(table_list, config, values){
  purrr::map(
    table_list,
    filter_list_column,
    config$team_filter_column,
    values
  )
}

