#' Create Plot Count Dataframe with Config
#' This function is used to create counts for histogram style plots if the
#' counting has to be done in a way that preserves certain groups that have a
#' count of 0.
#'
#' @param tbl A tibble
#' @param config A named list. This list must have a named list named
#' complete_count. This list must have lists named "factor columns" and
#' "complete_columns".
create_plot_count_df_with_config <- function(tbl, config){
  create_plot_count_df(
    tbl,
    config$complete_count$complete_columns
  )
}

#' Create Plot Count Dataframe
#' This function is used to create counts for histogram style plots if the
#' counting has to be done in a way that preserves certain groups that have a
#' count of 0.
#'
#' @param tbl A tibble
#' @param complete_columns A list of strings that are columns in the data. This
#' should be the aesthetic that is intended to be present in the plot even if
#' it has zero counts such as the x-axis, or possibly a facet.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
create_plot_count_df <- function(tbl, complete_columns){
  tbl %>%
    dplyr::mutate(dplyr::across(unlist(complete_columns), forcats::as_factor)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by_all() %>%
    dplyr::tally(., name = "Count") %>%
    dplyr::ungroup() %>%
    tidyr::complete(
      !!!rlang::syms(unlist(complete_columns)),
      fill = list("Count" = 0L)
    )
}
