create_synapse_login <- function(){
  if("use_conda_env.R" %in% list.files("R")) source("R/use_conda_env.R")
  synapseclient <- reticulate::import("synapseclient")
  syn <- synapseclient$Synapse()
  syn$login()
  return(syn)
}

get_synapse_tbl <- function(syn, table_id, columns = NULL, limit = NULL){
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
    syn$tableQuery() %>%
    purrr::pluck("filepath") %>%
    readr::read_csv(.) %>%
    dplyr::select(!dplyr::contains("depr")) %>%
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
