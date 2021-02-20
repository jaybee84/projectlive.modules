test_that("format_date_columns", {
  data1 <- dplyr::tibble("createdOn" = c(1.454526e+12, 1.454526e+12))
  data2 <- dplyr::tibble("year" = 2001L, "month" = "January")
  result1 <- format_date_columns(data1)
  result2 <- format_date_columns(data2)
  expect_named(result1, c("createdOn", "datetime", "date", "year", "month"))
  expect_named(result2, c("year", "month"))
})

test_that("filter_list_column",{
  data <- dplyr::tibble(
    "x" = c("a", "b", "c", NA, "c", "d"),
    "y" = list(
      "b",
      "a",
      NA,
      c("a", "c"),
      c("b", "c"),
      c("a", NA)
    )
  )
  result1 <- filter_list_column(data, "x", "a")
  expect_equal(result1, dplyr::tibble("x" = "a", "y" = list("b")))

  result2 <- filter_list_column(data, "y", "a")
  expect_equal(
    result2,
    dplyr::tibble(
      "x" = c("b", NA, "d"),
      "y" = list("a", c("a", "c"), c("a", NA))
    )
  )
})

test_that("summarise_df_counts",{
  data1 <- dplyr::tibble(
    "group_col" = c(rep("g1", 5), rep("g2", 3)),
    "count_col1" = c(rep("x", 2), rep("y", 6)),
    "count_col2" = c(rep("a", 1), rep("b", 7)),
    "col4" = "x"
  )
  group_column <-  "group_col"
  columns <-  list("group_col", "count_col1", "count_col2")

  result1 <- summarise_df_counts(data1, group_column, columns)
  expect_equal(
    result1,
    dplyr::tibble(
      "group_col" = c("g1", "g2"),
      "count_col1" = c(2, 1),
      "count_col2" = c(2, 1)
    )
  )

  data2 <- dplyr::tibble(
    "group_col" = character(),
    "count_col1" = character(),
    "count_col2" = character(),
    "col4" = character()
  )

  result2 <- summarise_df_counts(data2, group_column, columns)
  expect_equal(
    result2,
    dplyr::tibble(
      "group_col" = character(),
      "count_col1" = character(),
      "count_col2" = character()
    )
  )
})

test_that("create_merged_table_with_config", {
  config <- list(
    "join_column" = "studyName",
    "tables" = list("studies_table", "files_table"),
    "columns" =  list(
      list("studyName", "studyLeads"),
      list("studyName", "resourceType", "year", "assay")
    )
  )
  group_object <- list(
    "studies_table" = dplyr::tibble(
      "studyName" = c("s1", "s2"),
      "studyLeads" = c("l1", "l2"),
      "other_col" = c("x", "y")
    ),
    "files_table" = dplyr::tibble(
      "studyName" = c("s1", "s3"),
      "resourceType" = c("r1", "r2"),
      "year" = c(2001L, 2002L),
      "assay" = c("a1", "a2"),
      "other_col" = c("x", "y")
    )
  )
  result <- create_merged_table_with_config(group_object, config)
  expected_result <- dplyr::tibble(
    "studyName" = c("s1", "s2"),
    "studyLeads" = c("l1", "l2"),
    "resourceType" = c("r1", NA),
    "year" = c(2001L, NA),
    "assay" = c("a1", NA),
  )
  expect_equal(result, expected_result)
})

test_that("replace_values_if_col_value_in_list", {
  data <- dplyr::tribble(
    ~x,  ~y,  ~Count,
    "a", "a", 1L,
    "b", "b", 2L
  )
  expected_result1 <- dplyr::tribble(
    ~x,  ~y,  ~Count,
    NA,  "a", 1L,
    "b", "b", 2L
  )
  expect_equal(
    expected_result1,
    replace_values_if_col_value_in_list(data, "Count", 1L, "x")
  )
  expected_result2 <- dplyr::tribble(
    ~x,  ~y,  ~Count,
    NA,  NA,  1L,
    "b", "b", 2L
  )
  expect_equal(
    expected_result2,
    replace_values_if_col_value_in_list(data, "Count", 1L, c("x", "y"))
  )
  expected_result3 <- dplyr::tribble(
    ~x,             ~y,            ~Count,
    NA_character_,  NA_character_, 1L,
    NA_character_,  NA_character_, 2L
  )
  expect_equal(
    expected_result3,
    replace_values_if_col_value_in_list(data, "Count", c(1L, 2L), c("x", "y"))
  )
})

test_that("create_plot_count_df", {
  data <- dplyr::tribble(
    ~Lead,  ~fill,  ~Year,
    "a",    "a",    2000L,
    "a",    "a",    2000L,
    "a",    "a",    2001L,
    "b",    "a",    2001L,
    "c",    NA,     NA
  )

  expected_result1 <- dplyr::tribble(
    ~Lead,  ~Year,  ~fill, ~Count,
    "a",    2000L,  "a",   2L,
    "a",    2001L,  "a",   1L,
    "b",    2000L,  NA,    0L,
    "b",    2001L,  "a",   1L,
    "c",    2000L,  NA,    0L,
    "c",    2001L,  NA,    0L
  ) %>%
    dplyr::mutate("Lead" = forcats::as_factor(.data$Lead))

  expect_equal(
    create_plot_count_df(data, "Lead", c("Lead", "Year")),
    expected_result1
  )
})

test_that("create_data_focus_tables", {
  data <- dplyr::tribble(
    ~Study,  ~Assay, ~Resource, ~Year,
    "s1",    "a1",   "r1",      2001L,
    "s2",    "a2",   NA,        2002L
  )
  expected_results <- list(
    "Assay" = dplyr::tribble(
      ~Study,  ~Assay,
      "s1",    "a1",
      "s2",    "a2"
    ),
    "Resource" = dplyr::tribble(
      ~Study,  ~Resource,
      "s1",    "r1"
    )
  )
  expect_equal(
    expected_results,
    create_data_focus_tables(data, "Study", c("Assay", "Resource"))
  )
})


test_that("safe_pluck_list", {
  list1 = list(
    list("name" = "col1", "display_name" = "Column1", "type" = "x"),
    list("name" = "col2", "type" = "x")
  )
  res1 <- safe_pluck_list(list1, "name")
  res2 <- safe_pluck_list(list1, "display_name")
  expect_equal(res1, c("col1", "col2"))
  expect_equal(res2, c("Column1", NA))
})

test_that("rename_df_columns_with_config", {
  tbl1 <- dplyr::tibble("col1" = c(), "col2" = c(), "colNum3" = c())
  config1 <- list(
    "columns" = list(
      "col1" = list("name" = "col1", "display_name" = "Column1", "type" = "x"),
      "col2" = list("name" = "col2", "type" = "x"),
      "col3" = list("name" = "colNum3", "type" = "x")
    )
  )
  config2 <- list(
    "columns" = list(
      "col1" = list("name" = "col1", "display_name" = "Column1", "type" = "x"),
      list("name" = "col2", "type" = "x", "display_name" = "Column2"),
      list("name" = "colNum3", "type" = "x", "display_name" = "Column3")
    )
  )
  config3 <- list(
    "columns" = list(
      "col2" = list("name" = "col2", "type" = "x"),
      "col3" = list("name" = "colNum3", "type" = "x")
    )
  )
  res1 <- rename_df_columns_with_config(tbl1, config1)
  res2 <- rename_df_columns_with_config(tbl1, config2)
  res3 <- rename_df_columns_with_config(tbl1, config3)
  expect_equal(
    res1,
    dplyr::tibble("Column1" = c(), "Col2" = c(), "Colnum3" = c())
  )
  expect_equal(
    res2,
    dplyr::tibble("Column1" = c(), "Column2" = c(), "Column3" = c())
  )
  expect_equal(
    res3,
    dplyr::tibble("Col2" = c(), "Colnum3" = c())
  )
})

test_that("truncate_df_cols_with_config", {
  tbl1 <- dplyr::tibble(
    "col1" = c("aaaaaaaaaaaaaaaaaaaaaaa", "a", NA),
    "col2" = c("bbbbbbbbbbbbbbbbbbbbbbb", "b", NA)
  )
  config1 <- list(
    "columns" = list(
      "col1" = list(
        "name" = "col1",
        "type" = "character"
      ),
      "col2" = list(
        "name" = "col2",
        "type" = "character"
      )
    )
  )
  config2 <- list(
    "columns" = list(
      "col1" = list(
        "name" = "col1",
        "type" = "character",
        "truncate" = 6
      ),
      "col2" = list(
        "name" = "col2",
        "type" = "character"
      )
    )
  )
  config3 <- list(
    "columns" = list(
      "col1" = list(
        "name" = "col1",
        "type" = "character",
        "truncate" = 6
      ),
      "col2" = list(
        "name" = "col2",
        "type" = "character",
        "truncate" = 7
      )
    )
  )
  res1 <- truncate_df_cols_with_config(tbl1, config1)
  expect_equal(res1, tbl1)
  res2 <- truncate_df_cols_with_config(tbl1, config2)
  expect_equal(
    res2,
    dplyr::tibble(
      "col1" = c("aaa...", "a", NA),
      "col2" = c("bbbbbbbbbbbbbbbbbbbbbbb", "b", NA)
    )
  )
  res3 <- truncate_df_cols_with_config(tbl1, config3)
  expect_equal(
    res3,
    dplyr::tibble(
      "col1" = c("aaa...", "a", NA),
      "col2" = c("bbbb...", "b", NA)
    )
  )
})

test_that("add_distinct_values_from_columns", {
  tbl <- dplyr::tibble(
    "col1" = c("a", "a", "b"),
    "col2" = c("c", "d", "e"),
    "col3" = c("a", "b", "e")
  )
  expect_equal(add_distinct_values_from_columns(tbl, "col1"), 2)
  expect_equal(add_distinct_values_from_columns(tbl, "col2"), 3)
  expect_equal(add_distinct_values_from_columns(tbl, c("col1", "col2")), 5)
  expect_equal(add_distinct_values_from_columns(
    tbl, c("col1", "col2", "col3")), 8
  )
})

test_that("get_distinct_value_from_column", {
  df <- dplyr::tibble(
    "col1" = c("a", "a", "b"),
    "col2" = c("c", "d", "e")
  )
  expect_equal(get_distinct_value_from_column(df, "col1"), 2)
  expect_equal(get_distinct_value_from_column(df, "col2"), 3)
})


count_df <- dplyr::tribble(
  ~studyName, ~name,          ~value,                   ~count,
  "s1",       "assay",        "immunohistochemistry",   395L,
  "s1",       "resourceType", "experimentalData",       416L,
  "s1",       "resourceType", "report",                 12L,
  "s1",       "species",      "Human",                  421L,
  "s1",       "tumorType",    "Cutaneous Neurofibroma", 387L
)

test_that("create_plot_df_from_count_df ", {
  result1 <- create_plot_df_from_count_df("assay", "Assays", count_df)
  expect_named(result1, c("studyName", "Assays", "assay"))
  expect_equal(nrow(result1), 1)
  expect_equal(result1$Assays, 395L)
  result2 <- create_plot_df_from_count_df("resourceType", "Resources", count_df)
  expect_named(result2, c("studyName", "Resources", "resourceType"))
  expect_equal(nrow(result2), 2)
  expect_equal(result2$Resources, c(416L, 12L))
})

test_that("create_plot_dfs_from_count_df ", {
  columns       <- c("assay", "resourceType")
  count_columns <- c("Assays", "Resources")

  result <- create_plot_dfs_from_count_df(columns, count_columns, count_df)

  expect_named(result[[1]], c("studyName", "Assays", "assay"))
  expect_named(result[[2]], c("studyName", "Resources", "resourceType"))

  expect_equal(nrow(result[[1]]), 1)
  expect_equal(nrow(result[[2]]), 2)

  expect_equal(result[[1]]$Assays, 395L)
  expect_equal(result[[2]]$Resources, c(416L, 12L))
})


