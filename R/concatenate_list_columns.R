# Concatenate All Dataframe List Columns
#
#' This function will concatenate all list columns into character columns.
#'
#' @param tbl A Tibble
concatenate_all_list_columns <- function(tbl){
  dplyr::mutate(
    tbl,
    dplyr::across(
      where(purrr::is_list),
      ~purrr::map_chr(.x, concatenate_list)
    )
  )
}

#' Concatenate Dataframe List Columns With Config
#'
#' This function will concatenate list columns into character columns.
#' Any column of type "list:character" will be concatenated
#'
#' @param tbl A Tibble
#'
#'   data <- dplyr::tribble(
#'    ~study,       ~month,
#'    c("s1", "s2)  "January"
#'  )
#'
#' @param config A list with a named list named "columns" that has an entry
#' for each column needed in the tibble. Each column must have a "name", and
#' "type" field.
#'
#'   config <- list(
#'    "columns" = list(
#'       list(
#'         "name" = "study",
#'         "display_name" = "Consortium",
#'         "type" = "list:character"
#'       ),
#'      )
#'     )
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
concatenate_df_list_columns_with_config <- function(tbl, config){
  list_columns <- config %>%
    purrr::pluck("columns") %>%
    dplyr::tibble(
      "type" = safe_pluck_list(., "type"),
      "name" = safe_pluck_list(., "name")
    ) %>%
    dplyr::select("type", "name") %>%
    dplyr::filter(.data$type == "list:character") %>%
    dplyr::pull("name") %>%
    unname()

  concatenate_list_columns(tbl, list_columns)
}

#' Concatenate List Columns
#' This function will concatenate list columns into character columns
#'
#' @param tbl A Tibble
#' @param columns A list of strings that are names of columns in data to
#' be concatenated
concatenate_list_columns <- function(tbl, columns){
  dplyr::mutate_at(
    tbl,
    columns,
    ~purrr::map_chr(.x, concatenate_list)
  )
}

concatenate_list <- function(lst){
  if(is.null(list)) return(NA)
  if(all(is.na(lst))) return(NA)
  lst %>%
    sort() %>%
    stringr::str_c(collapse = " | ")
}


