#' Format Date Columns
#' This function adds date, year and month columns if the the input has a
#' createdOn column from Synapse.
#' If the the input has year or month columns, those are converted to factors.
#' @param data A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
format_date_columns <- function(data){
  if (!"createdOn" %in% colnames(data)){
    if ("year" %in% colnames(data)){
      data <- dplyr::mutate(data, "year" = forcats::as_factor(.data$year))
    }
    if ("month" %in% colnames(data)){
      data <- dplyr::mutate(data, "month" = forcats::as_factor(.data$month))
    }
  }
  else{
    data <- data %>%
      dplyr::mutate(
        "datetime" = lubridate::as_datetime(
          .data$createdOn/1000, origin = "1970-01-01"
        ),
        "date" = lubridate::ymd(lubridate::floor_date(.data$datetime, "day")),
        "year" = forcats::as_factor(lubridate::year(.data$datetime)),
        "month" = lubridate::month(.data$datetime, label = TRUE, abbr = TRUE)
      )
  }
  return(data)
}

summarise_df_counts <- function(data, group_column, columns){
  result <- dplyr::select(data, dplyr::all_of(unlist(columns)))

  if(nrow(result) == 0) return(result)

  result %>%
    tidyr::unnest(dplyr::everything()) %>%
    dplyr::group_by_at(group_column) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        dplyr::n_distinct,
        na.rm = T
      )
    ) %>%
    dplyr::ungroup()
}


#' Filter List Column
#' This function filters a list column in the input data. Rows are kept if all
#' items in the values are in the supplied column.
#'
#' @param data A Tibble
#' @param column A string, that is the name of a column in the data
#' @param values A list of values
filter_list_column <- function(data, column, values){
  dplyr::filter(
    data,
    purrr::map_lgl(!!rlang::sym(column), ~all(values %in% .x))
  )
}

#' Create Merged Table With Config
#'
#' @param group_object A named list with tables
#' @param config A named list. It must have the names "tables", "columns",
#' and "join_column". The tables value must be names of tables in the group
#' object. "join_column" must be a column in all tables. "columns" must be a
#' list of lists, one per table. Each list must be a subset of that tables
#' column names.
#' @importFrom magrittr %>%
create_merged_table_with_config <- function(group_object, config){
  group_object %>%
    magrittr::extract(unlist(config$tables)) %>%
    purrr::map2(
      purrr::map(config$columns, unlist),
      dplyr::select
    ) %>%
    purrr::reduce(dplyr::left_join, by = config$join_column)
}


#' Replace Values If Column Values In List
#' This function replaces values in a list of columns, with a value, if a
#' given column has a supllied value.
#'
#' @param data A Tibble
#' @param column A string which is a name of a column in the data to check the
#' values in the lst
#' @param lst A list of values to check the column against
#' @param columns A list of strings that are column names.
#' @param replace_value A value to replace
#' @importFrom rlang !!
replace_values_if_col_value_in_list <- function(
  data, column, lst, columns, replace_value = NA_character_
){
  dplyr::mutate(
    data,
    dplyr::across(
      columns,
      ~dplyr::if_else(!!rlang::sym(column) %in% lst, replace_value, .x)
    )
  )
}

#' Create Data Focus Tables
#' This function creates a list of tables from on input tibble. The list will
#' have one table per column listed in the fill_columns list. This function is
#' used in the study_summary module to create the data_focus plots.
#'
#' @param data A tibble
#' @param x_column A string that is the name of a column in the data
#' @param fill_columns A list of strings that are names of columns in data
#' @importFrom magrittr %>%
create_data_focus_tables <- function(data, x_column, fill_columns){
  res <-
    purrr::map(
      fill_columns,
      ~dplyr::select(data, dplyr::all_of(x_column), .x)
    ) %>%
    purrr::set_names(fill_columns) %>%
    purrr::map(tidyr::drop_na) %>%
    purrr::discard(., purrr::map(., nrow) == 0)
}

#' Safe Pluck List
#' This function is used to safely pluck named value from a list of named lists.
#' This will return a vector of values the length of the list. If the named list
#' doesn't have the name, an NA will be returned.
#'
#' @param lst A list of named lists
#'
#' list1 = list(
#'  list("name" = "col1", "display_name" = "Column1", "type" = "x"),
#'  list("name" = "col2", "type" = "x")
#' )
#'
#' @param n A string. If the string is a name in the named lists, the value of
#' that string will be returned, otherwise NA.
#' @importFrom magrittr %>%
safe_pluck_list <- function(lst, n){
  lst %>%
    purrr::map(purrr::pluck, n, .default = NA) %>%
    unlist()
}

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
#' for each column needed in the tibble. Each column must have a "name",
#' and "type" field. "display_name" will be used to rename the column.
#'
#'   config <- list(
#'    "columns" = list(
#'       list(
#'         "name" = "study",
#'         "display_name" = "Consortium",
#'         "type" = "character"
#'       ),
#'      list(
#'         "name" = "month",
#'         "display_name" = "Month",
#'         "type" = "integer"
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

  dplyr::select(tbl, dplyr::all_of(column_select_list))
}

truncate_df_cols_with_config <- function(tbl, config){
  config <- config %>%
    purrr::pluck("columns") %>%
    purrr::keep(
      .,
      purrr::map_lgl(., ~is.numeric(purrr::pluck(.x, "truncate")))
    )

  cols <- purrr::map_chr(config, purrr::pluck, "name")
  widths <- config %>%
    purrr::map_dbl(purrr::pluck, "truncate") %>%
    as.integer() %>%
    purrr::set_names(cols)

  dplyr::mutate(
    tbl,
    dplyr::across(
      cols,
      ~ stringr::str_trunc(.x, width = widths[dplyr::cur_column()])
    )
  )
}

#' Get Number Of Distinct Values From Column
#' This function returns the number of distinct values, including NA's in
#' a column
#'
#' @param tbl A Tibble
#' @param column A string that is the name of a column in the tibble
#' @importFrom magrittr %>%
get_distinct_value_from_column <- function(tbl, column){
  tbl %>%
    dplyr::pull(column) %>%
    dplyr::n_distinct()
}

#' Add Distinct Values From Columns
#' This function find sthe number of distinct values from one or more columns
#' and returns the sum of those.
#'
#' @param tbl A tibble
#' @param columns a list of strings that are names of columns in the tibble
#' @importFrom magrittr %>%
add_distinct_values_from_columns <- function(tbl, columns){
  result <-
    purrr::map_int(columns, ~get_distinct_value_from_column(tbl, .x)) %>%
    sum()
}

#' Create Plot Dataframe From Count Dataframe
#' This function creates a summary of the name and value column.
#'
#' The name column is filtered for only values that equal the column_value.
#'
#' The name and value column are pivoted so that a new column is created that
#' has the name of the column value. The number of rows will equal the number
#' of unique value in the value column, where the name column is equal to
#' the column_value
#'
#' Finally the counts column is renamed the using the count_column
#'
#'
#' @param column A value that exists in the "name" column of the data
#' @param count_column A string that is the new column name
#' @param data A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang := .data
#' @examples
#'  data <- dplyr::tribble(
#'  ~studyName, ~name,          ~value,                   ~count,
#'  "s1",       "assay",        "immunohistochemistry",   395L,
#'  "s1",       "resourceType", "experimentalData",       416L,
#'  "s1",       "resourceType", "report",                 12L,
#'  "s1",       "species",      "Human",                  421L,
#'  "s1",       "tumorType",    "Cutaneous Neurofibroma", 387L
#' )
#' create_plot_df_from_count_df("assay", "Assays", data)
#' @export
create_plot_df_from_count_df <- function(column, count_column, data){
  data %>%
    dplyr::filter(.data$name == column) %>%
    tidyr::pivot_wider() %>%
    dplyr::rename(!!rlang::ensym(count_column) := "count")
}

#' Create Plot Dataframes From Count Dataframe
#' See create_plot_df_from_count_df for outputs
#' @param column_values A list of values that exists in the "name" column
#' of the data
#' @param count_columns A list of strings that will the new column names
#' @param data A tibble
create_plot_dfs_from_count_df <- function(column_values, count_columns, data){
  purrr::map2(
    column_values,
    count_columns,
    create_plot_df_from_count_df,
    data
  )
}

