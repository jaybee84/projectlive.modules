test_that("create_plot_count_df", {
  tbl <- dplyr::tribble(
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
    create_plot_count_df(tbl, "Lead", c("Lead", "Year")),
    expected_result1
  )
})

test_that("create_plot_count_df_with_config", {
  tbl <- dplyr::tribble(
    ~Lead,  ~fill,  ~Year,
    "a",    "a",    2000L,
    "a",    "a",    2000L,
    "a",    "a",    2001L,
    "b",    "a",    2001L,
    "c",    NA,     NA
  )

  config <- list(
    "complete_count" = list(
      "factor_columns" = list("Lead"),
      "complete_columns" = list("Lead", "Year")
    )
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
    create_plot_count_df_with_config(tbl, config),
    expected_result1
  )
})

