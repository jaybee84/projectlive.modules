#' Select Dataframe Columns With Config
#'
#' This function will select columns based on the Config.
#' Only columns in the Config will be selected.
#'
#' @param tbl A tibble
#'
#'  tbl <- dplyr::tribble(
#'    ~study, ~month,    ~year,
#'    "s1",   "January", 2001L
#'  )
#' @param config A list with a named list named "columns" that has an entry
#' for each column needed in the tibble. Each column must have a "name" field.
#'
#'   config <- list(
#'     "columns" = list(
#'       list("name" = "study"),
#'       list("name" = "month")
#'     )
#'   )
#' @importFrom magrittr %>%
select_df_columns_with_config <- function(tbl, config){

  columns<- config %>%
    purrr::pluck("columns") %>%
    purrr::map_chr(purrr::pluck, "name")

  dplyr::select(tbl, dplyr::all_of(columns))
}
