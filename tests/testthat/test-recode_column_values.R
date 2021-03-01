test_that("recode_column_values", {
  tbl1 <- dplyr::tribble(
    ~col1, ~col2,
    "a",   "c",
    "a",   "d",
    "b",   NA
  )
  lst1 <- list("a" = "x", "b" = "y", "c" = "z")
  col1 <- "col1"
  col2 <- "col2"

  res1 <- recode_column_values(tbl1, col1, lst1)
  expect_equal(
    res1,
    dplyr::tibble("col1" = c("x", "x", "y"), "col2" = c("c", "d", NA))
  )
  res2 <- recode_column_values(tbl1, col2, lst1, .missing = "missing")
  expect_equal(
    res2,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("z", "d", "missing"))
  )
  res3 <- recode_column_values(tbl1, col2, list(), .missing = "missing")
  expect_equal(
    res3,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", "missing"))
  )
  res4 <- recode_column_values(tbl1, col2, lst = NULL, .missing = "missing")
  expect_equal(
    res4,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", "missing"))
  )
  res5 <- recode_column_values(tbl1, col2)
  expect_equal(
    res5,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", NA))
  )

  tbl2 <- dplyr::tibble(
    "col1" = list(c("a", "b", "c", "d", NA), NA),
    "col2" = c(2001L, 2002L),
    "col3" = factor("x", "y")
  )

  col1 <- "col1"
  col2 <- "col2"
  col3 <- "col3"

  expect_error(
    recode_column_values(tbl2, col1,),
    "Cannot recode list column: col1"
  )

  expect_error(
    recode_column_values(tbl2, col2,),
    "Cannot recode non-character column: col2"
  )

  expect_error(
    recode_column_values(tbl2, col3,),
    "Cannot recode non-character column: col3"
  )
})


test_that("get_n_most_common_values", {
  tbl1 <- dplyr::tibble(
    "col1" = c("a", "a", "b", "b", "c"),
    "col2" = c("b", "b", "a", "a", "c"),
    "col3" = c(1L, 1L, 2L, 2L, 3L),
    "col4" = factor(c(1L, 1L, 2L, 2L, 3L))
  )

  actual1   <- get_n_most_common_values(tbl1, "col1", 1)
  expected1 <- "a"
  expect_equal(actual1, expected1)

  actual2   <- get_n_most_common_values(tbl1, "col1", 2)
  expected2 <- c("a", "b")
  expect_equal(actual2, expected2)

  actual3   <- get_n_most_common_values(tbl1, "col1", 5)
  expected3 <- c("a", "b", "c")
  expect_equal(actual3, expected3)

  actual4   <- get_n_most_common_values(tbl1, "col2", 1)
  expected4 <- "a"
  expect_equal(actual4, expected4)

  actual5   <- get_n_most_common_values(tbl1, "col3", 1)
  expected5 <- 1L
  expect_equal(actual5, expected5)

  actual6   <- get_n_most_common_values(tbl1, "col4", 1)
  expected6 <- factor(1L, levels = c(1L, 2L, 3L))
  expect_equal(actual6, expected6)
})

test_that("recode_df_with_config", {
  tbl1 <- dplyr::tibble(
    "col1" = c("a", NA, "b"),
    "col2" = c("c", "d", NA),
    "col3" = c(1L, 2L, NA),
    "col4" = c(T, F, F),
    "col5" = c("", "", ""),
  )
  config1 <- list(
    "columns" = list(
      list(
        "name" = "col1",
        "recode" = list(
          "replace_values" = list("a" = "x")
        )
      ),
      list(
        "name" = "col2",
        "recode" = list(
          "replace_values" = list("c" = "z"),
          "na_replace" = "M",
          "default_replace" = "O"
        )
      ),
      list("name" = "col3"),
      list("name" = "col4" ),
      list(
        "name" = "col5",
        "recode" = list(
          "na_replace" = "Blank"
        )
      )
    )
  )
  res1 <- recode_df_with_config(tbl1, config1)
  expect_equal(
    res1,
    dplyr::tibble(
      "col1" = c("x", NA, "b"),
      "col2" = c("z", "O", "M"),
      "col3" = c(1L, 2L, NA),
      "col4" = c(T, F, F),
      "col5" = c("Blank", "Blank", "Blank"),
    )
  )

  config2 <- list(
    "columns" = list(
      list(
        "name" = "col1",
        "recode" = list(
          "top_values" = 1L,
          "na_replace" = "Missing",
          "default_replace" = "Other"
        )
      )
    )
  )

  actual2   <- recode_df_with_config(tbl1, config2)
  expected2 <- dplyr::tibble(
    "col1" = c("a", "Missing", "Other"),
    "col2" = c("c", "d", NA),
    "col3" = c(1L, 2L, NA),
    "col4" = c(T, F, F),
    "col5" = c("", "", ""),
  )

  expect_equal(actual2, expected2)

})

