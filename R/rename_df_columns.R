#' Rename Dataframe Columns With Config
#'
#' This function will select and rename columns based on the Config.
#' Only columns in the Config will be selected. Columns with a display
#' name, will be renamed that, otherwise stringr::str_to_title will be used on
#' the name.
#'
#' @param tbl A tibble
#'
#'  data <- dplyr::tribble(
#'    ~study, ~month,    ~year,
#'    "s1",   "January", 2001L
#'  )
#' @param config A list with a named list named "columns" that has an entry
#' for each column needed in the tibble. Each column must have a "name" field.
#'"display_name" will be used to rename the column.
#'
#'   config <- list(
#'    "columns" = list(
#'       list(
#'         "name" = "study",
#'         "display_name" = "Consortium"
#'       ),
#'      list(
#'         "name" = "month",
#'         "display_name" = "Month"
#'       ),
#'      )
#'     )
#' @importFrom magrittr %>%
#' @importFrom rlang .data
rename_df_columns_with_config <- function(tbl, config){
  column_select_list <- config %>%
    purrr::pluck("columns") %>%
    dplyr::tibble(
      "display_name" = safe_pluck_list(., "display_name"),
      "name" = safe_pluck_list(., "name")
    ) %>%
    dplyr::select("display_name", "name") %>%
    dplyr::mutate(
      "display_name" = as.character(.data$display_name),
      "display_name" = dplyr::if_else(
        is.na(.data$display_name),
        stringr::str_to_title(.data$name),
        .data$display_name
      )) %>%
    tibble::deframe(.)

  dplyr::rename(tbl, column_select_list)
}
