test_that("create_count_column", {
  tbl <- dplyr::tribble(
    ~Lead,  ~fill,  ~Year,
    "a",    "x",    2000L,
    "a",    "x",    2000L,
    "a",    "x",    2001L,
    "b",    "x",    2001L,
    "c",    NA,     NA
  )

  expected_result1 <- dplyr::tribble(
    ~Lead,  ~Year,  ~fill, ~Count,
    "a",    2000L,  "x",   2L,
    "a",    2001L,  "x",   1L,
    "b",    2000L,  NA,    0L,
    "b",    2001L,  "x",   1L,
    "c",    2000L,  NA,    0L,
    "c",    2001L,  NA,    0L
  ) %>%
    dplyr::mutate(
      "Lead" = forcats::as_factor(.data$Lead),
      "Year" = forcats::as_factor(.data$Year)
    )

  expect_equal(
    create_count_column(tbl, complete_columns = c("Lead", "Year")),
    expected_result1
  )


  result2 <- create_count_column(tbl)

  expected2 <- dplyr::tribble(
    ~Lead,  ~fill, ~Year,  ~Count,
    "a",    "x",   2000L,  2L,
    "a",    "x",   2001L,  1L,
    "b",    "x",   2001L,  1L
  )

  expect_equal(result2, expected2)
})

test_that("create_count_column2", {
  tbl <- dplyr::tibble(
    "Lead" = character(),
    "Year" = integer(),
    "Col" = character()
  )

  tbl <- dplyr::tibble(
    "Lead" = factor(),
    "Year" = factor(),
    "Col" = character(),
    "Count" = integer()
  )

  actual <- create_count_column(tbl, complete_columns = c("Lead", "Year"))

  expect_equal(actual, tbl)

})

test_that("create_count_column_with_config", {
  tbl <- dplyr::tribble(
    ~Lead,  ~fill,  ~Year,
    "a",    "x",    2000L,
    "a",    "x",    2000L,
    "a",    "x",    2001L,
    "b",    "x",    2001L,
    "c",    NA,     NA
  )

  config <- list(
    "count_column" = list(
      "complete_columns" = list("Lead", "Year")
    )
  )

  expected_result1 <- dplyr::tribble(
    ~Lead,  ~Year,  ~fill, ~Count,
    "a",    2000L,  "x",   2L,
    "a",    2001L,  "x",   1L,
    "b",    2000L,  NA,    0L,
    "b",    2001L,  "x",   1L,
    "c",    2000L,  NA,    0L,
    "c",    2001L,  NA,    0L
  ) %>%
    dplyr::mutate(
      "Lead" = forcats::as_factor(.data$Lead),
      "Year" = forcats::as_factor(.data$Year)
    )

  expect_equal(
    create_count_column_with_config(tbl, config),
    expected_result1
  )
})
