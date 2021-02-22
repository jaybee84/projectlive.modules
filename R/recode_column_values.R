#' Recode Dataframe With Config
#' This function replaces values in a character column. If the Config
#' has "replace_values" field, this will be the mapping used. If the parameter
#' list has a "deafult_replace", anything not in the replace values list will be
#' replaced by this value. If the Config has a "na_replace" field NA
#' values will be replaced by this.
#'
#' It is assumed that this table has been run through
#' concatenate_df_list_columns_with_config, as columns of type character,
#' and those that were of list:character will be recoded.
#'
#' @param tbl A tibble
#'
#'  data <-  dplyr::tribble(
#'   ~col1, ~col2,
#'   "a",   "c",
#'   "a",   "d",
#'   "b",   NA
#'  )
#'
#' @param config A list with a named list named "columns" that has an entry
#' for each column needed in the tibble. Each column must have a "name",
#' and "type" field. Optional fields include "replace_values", "display_name",
#' "na_replace", and "deafult_replace".
#'
#'   config1 <- list(
#'    "columns" = list(
#'      "col1" = list(
#'        "name" = "col1",
#'        "type" = "character",
#'        "replace_values" = list(
#'          "a" = "A"
#'        )
#'      ),
#'      "col2" = list(
#'        "name" = "col2",
#'        "type" = "character",
#'        "replace_values" = list(
#'          "c" = "C"
#'         ),
#'        "na_replace" = "Missing",
#'        "default_replace" = "Other"
#'      )
#'     )
#' @importFrom magrittr %>%
#' @export
recode_df_with_config <- function(tbl, config){

  column_config <- config %>%
    purrr::pluck("columns") %>%
    purrr::keep(
      .,
      !purrr::map_lgl(purrr::map(., purrr::pluck, "recode"), is.null)
    )

  for (config in column_config) {
    tbl <- recode_column_values(
      tbl,
      config$name,
      config$recode$replace_values,
      .default = config$recode$default_replace,
      .missing = config$recode$na_replace
    )
  }
  return(tbl)
}

#' Recode Column Values
#' This function will recode a column of values in a tibble based o the list
#' passed. If no list is passed none of the values will be replaces unless
#' optional arguments are passed via ... to dplyr::recode such as .default
#' or .missing
#'
#' @param tbl A tibble
#'
#' data <-  dplyr::tribble(
#'  ~col1, ~col2,
#'  "a",   "c",
#'  "a",   "d",
#'  "b",   NA
#')
#'
#' @param column A string that is the name of a column in the tbl that will be
#' recoded
#' @param lst A named list where each name is a possible value in the column,
#' and the values are their replacements
#'
#' lst <- list("a" = "x", "b" = "y", "c" = "z")
#'
#' @param ... Other arguments to dplyr::recode
recode_column_values <- function(tbl, column, lst = NULL, ...){
  if(typeof(tbl[[column]]) == "list"){
    stop("Cannot recode list column: ", column)
  }
  if(is.null(lst)) lst <- list("0" = "0")
  col_var <- rlang::sym(column)
  tbl <- dplyr::mutate(tbl, !!col_var := dplyr::if_else(
    !!col_var == "",
    NA_character_,
    !!col_var
  ))
  dplyr::mutate(tbl, !!col_var := dplyr::recode(!!col_var, !!!lst, ...))
}
