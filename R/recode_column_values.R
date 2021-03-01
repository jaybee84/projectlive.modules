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
#' for each column needed in the tibble. Each column must have a "name", field.
#' If the config has a "recode" field, this function will be used. The recode
#' field should be a named list with the following optional fields:
#'
#' - "replace_values"
#' - "na_replace"
#' - "deafult_replace"
#'
#'   config <- list(
#'    "columns" = list(
#'      "col1" = list(
#'        "name" = "col1",
#'        "type" = "character",
#'        "recode" = list(
#'          "replace_values" = list("a" = "A")
#'        )
#'      ),
#'      "col2" = list(
#'        "name" = "col2",
#'        "type" = "character",
#'        "recode" = list(
#'          "replace_values" = list("c" = "C"),
#'          "na_replace" = "Missing",
#'          "default_replace" = "Other"
#'        )
#'      )
#'     )
#' @importFrom magrittr %>%
#' @export
recode_df_with_config <- function(tbl, config){

  column_configs <- config %>%
    purrr::pluck("columns") %>%
    purrr::keep(
      .,
      !purrr::map_lgl(purrr::map(., purrr::pluck, "recode"), is.null)
    )

  for (config in column_configs) {
    tbl <- recode_column_with_config(tbl, config)
  }
  return(tbl)
}

#' Recode Column With Config
#'
#' @param tbl A Tibble
#' @param config A named list
#'
#' @importFrom magrittr %>%
recode_column_with_config <- function(tbl, config){
  top_values <- config$recode$top_values
  replace_values <- config$recode$replace_values

  if(all(!is.null(top_values), !is.null(replace_values))){
    stop("Can not recode column with both fields top_values and repalce_values")
  }
  if(!is.null(top_values)){
    replace_values <- tbl %>%
      get_n_most_common_values(config$name, config$recode$top_values) %>%
      purrr::set_names(., .) %>%
      as.list()
  }
  recode_column_values(
    tbl,
    config$name,
    replace_values,
    .default = config$recode$default_replace,
    .missing = config$recode$na_replace
  )
}

#' Get N Most Common Values
#'
#' @param tbl A tibble
#' @param column A string, that is a column in the tbl
#' @param n an integer
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_n_most_common_values <- function(tbl, column, n = 5){
  col <- rlang::sym(column)

  tbl %>%
    dplyr::group_by(!!col) %>%
    dplyr::summarise("count" = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(.data$count), !!col) %>%
    dplyr::slice(1:n) %>%
    dplyr::pull(column)
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
  column_type <- typeof(tbl[[column]])
  if(column_type == "list"){
    stop("Cannot recode list column: ", column)
  }
  if(column_type != "character"){
    stop("Cannot recode non-character column: ", column)
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



