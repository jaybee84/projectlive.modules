test_that("create_plot_count_df", {
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
    create_plot_count_df(tbl, c("Lead", "Year")),
    expected_result1
  )
})

test_that("create_plot_count_df_with_config", {
  tbl <- dplyr::tribble(
    ~Lead,  ~fill,  ~Year,
    "a",    "x",    2000L,
    "a",    "x",    2000L,
    "a",    "x",    2001L,
    "b",    "x",    2001L,
    "c",    NA,     NA
  )

  config <- list(
    "complete_count" = list(
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
    create_plot_count_df_with_config(tbl, config),
    expected_result1
  )
})

