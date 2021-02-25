#' Create Plot Count Dataframe with Config
#' This function is used to create a count column.
#'
#' @param tbl A tibble
#' @param config A named list. This list must have a named list named
#' create_count_column. This list must have names "name" and "complete_columns"
create_count_column_with_config <- function(tbl, config){
  if(is.null(config$count_column)){
    return(tbl)
  }
  create_count_column(
    tbl,
    config$count_column$name,
    config$count_column$complete_columns
  )
}

#' Create Count Column
#' This function is used to create a count column.
#'
#' @param tbl A tibble.
#' @param count_column A string.
#' @param complete_columns A list of strings that are columns in the data. This
#' should be the aesthetic that is intended to be present in the plot even if
#' it has zero counts such as the x-axis, or possibly a facet.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
create_count_column <- function(tbl, count_column = NULL, complete_columns = NULL){
  if(is.null(count_column)) count_column <- "Count"
  tbl %>%
    dplyr::mutate(dplyr::across(unlist(complete_columns), forcats::as_factor)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by_all() %>%
    dplyr::tally(., name = count_column) %>%
    dplyr::ungroup() %>%
    tidyr::complete(
      !!!rlang::syms(unlist(complete_columns)),
      fill = list("Count" = 0L)
    )
}
