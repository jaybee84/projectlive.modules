create_internal_tracking_datatable <- function(tbl, config){
  date_column        <- rlang::sym(config$date_estimate_column)
  milestone_column   <- rlang::sym(config$milestone_column)

  tbl %>%
    dplyr::filter(!is.na(!!date_column)) %>%
    dplyr::select(!!milestone_column, !!date_column) %>%
    dplyr::arrange(!!date_column) %>%
    dplyr::distinct()
}

filter_internal_data_tbl <- function(tbl, config, milestone, join_column_string){
  join_column      <- rlang::sym(join_column_string)
  milestone_column <- rlang::sym(config$milestone_column)
  expected_column  <- rlang::sym(config$expected_files_column)

  tbl %>%
    dplyr::filter(!!milestone_column == milestone) %>%
    dplyr::select(
      !!join_column,
      !!milestone_column,
      !!expected_column
    )
}

filter_files_tbl <- function(tbl, config, date_range_start, date_range_end, join_column_string){
  join_column   <- rlang::sym(join_column_string)
  date_column   <- rlang::sym(config$date_created_column)
  actual_column <- rlang::sym(config$actual_files_column)

  tbl %>%
    dplyr::filter(
      !!date_column < date_range_end,
      !!date_column > date_range_start
    ) %>%
    dplyr::group_by(!!join_column) %>%
    dplyr::summarise(!!actual_column := dplyr::n())
}

filter_files_tbl2 <- function(tbl, config, milestone, join_column_string){
  join_column      <- rlang::sym(join_column_string)
  actual_column    <- rlang::sym(config$actual_files_column)
  milestone_column <- rlang::sym(config$milestone_column)

  tbl %>%
    dplyr::filter(!!milestone_column == milestone) %>%
    dplyr::group_by(!!join_column) %>%
    dplyr::summarise(!!actual_column := dplyr::n())
}

merge_tbls <- function(id_tbl, files_tbl, config, join_column_string){
  join_column      <- rlang::sym(join_column_string)
  date_column      <- rlang::sym(config$date_created_column)
  actual_column    <- rlang::sym(config$actual_files_column)
  expected_column  <- rlang::sym(config$expected_files_column)

  id_tbl %>%
    dplyr::full_join(files_tbl, by = join_column_string) %>%
    dplyr::mutate(
      !!actual_column := dplyr::if_else(
        is.na(!!actual_column),
        0L,
        !!actual_column
      )
    ) %>%
    dplyr::select(!!join_column, !!expected_column, !!actual_column) %>%
    tidyr::pivot_longer(
      cols = -c(!!join_column),
      names_to = "Types of Files",
      values_to = "Number of Files"
    ) %>%
    dplyr::mutate(
      "Types of Files" = base::factor(
        .data$`Types of Files`,
        levels = c(
          config$expected_files_column, config$actual_files_column
        )
      )
    )
}

# merge_tbls2 <- function(id_tbl, files_tbl, config, join_column_string){
#   join_column      <- rlang::sym(join_column_string)
#   date_column      <- rlang::sym(config$date_created_column)
#   actual_column    <- rlang::sym(config$actual_files_column)
#   expected_column  <- rlang::sym(config$expected_files_column)
#
#   id_tbl %>%
#     dplyr::full_join(files_tbl, by = join_column_string) %>%
#     dplyr::mutate(
#       !!actual_column := dplyr::if_else(
#         is.na(!!actual_column),
#         0L,
#         !!actual_column
#       )
#     ) %>%
#     dplyr::select(!!join_column, !!expected_column, !!actual_column) %>%
#     tidyr::pivot_longer(
#       cols = -c(!!join_column),
#       names_to = "Types of Files",
#       values_to = "Number of Files"
#     ) %>%
#     dplyr::mutate(
#       "Types of Files" = base::factor(
#         .data$`Types of Files`,
#         levels = c(
#           config$expected_files_column, config$actual_files_column
#         )
#       )
#     )
#
#   filtered_id_tbl2() %>%
#     dplyr::full_join(filtered_files_tbl2(), by = input$join_column_choice) %>%
#     dplyr::mutate(
#       !!actual_column := dplyr::if_else(
#         is.na(!!actual_column),
#         0L,
#         !!actual_column
#       )
#     ) %>%
#     dplyr::select(!!join_column, !!expected_column, !!actual_column) %>%
#     tidyr::pivot_longer(
#       cols = -c(!!join_column),
#       names_to = "Types of Files",
#       values_to = "Number of Files"
#     ) %>%
#     dplyr::mutate(
#       "Types of Files" = base::factor(
#         .data$`Types of Files`,
#         levels = c(
#           config$expected_files_column, config$actual_files_column
#         )
#       )
#     )
# }


