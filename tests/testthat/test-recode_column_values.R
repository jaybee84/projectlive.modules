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
    "col1" =  list(c("a", "b", "c", "d", NA), NA),
    "col2" = c("a", "d"),
    "col3" = c("x", "x")
  )
  expect_error(
    recode_column_values(
      tbl2, col1, lst1
    ),
    "Cannot recode list column: col1"
  )
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
})

