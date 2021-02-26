#' Format Plot Data With Config
#'
#' This function runs a tibble through the main data cleanup functions before
#' being displayed in a plot or data table. See those functions for more
#' details.
#'
#' @param tbl A Tibble
#'
#'   data <- dplyr::tribble(
#'    ~consortium, ~year, ~month,
#'    NA,          2000L, NA,
#'    "c1",        20001, "January"
#'  )
#'
#' @param config A named list.
#'
#' The list must contained an item named "columns" that has an entry
#' for each column needed in the tibble. Each column must have a "name",
#' and "type" field. Optional fields include "replace_values", "display_name",
#' "na_replace", and "default_replace".
#'
#' The list may also contain an item named drop_na. If the value of drop_na
#' is True, all rows with na values will be droped.
#'
#' The list may also contain an item named unlist_columns. All columns in this
#' list will be un-listed with tidyr::unnest()
#'
#'   config <- list(
#'    "columns" = list(
#'       list(
#'         "name" = "consortium",
#'         "display_name" = "Consortium",
#'         "na_replace" = "Not Applicable",
#'         "type" = "character"
#'       ),
#'      )
#'     )
#'
#' @importFrom magrittr %>%
#' @export
format_plot_data_with_config <- function(tbl, config){
  result <- tbl %>%
    select_df_columns_with_config(config) %>%
    tidyr::unnest(unlist(config$unlist_columns)) %>%
    concatenate_all_list_columns() %>%
    recode_df_with_config(config) %>%
    truncate_df_cols_with_config(config) %>%
    create_count_column_with_config(config)

  if(!is.null(config$drop_na) && config$drop_na){
    result <- tidyr::drop_na(result)
  }

  rename_df_columns_with_config(result, config)
}
