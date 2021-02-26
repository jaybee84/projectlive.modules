test_that("format_plot_data_with_config1", {
  config1 <- list(
    "columns" = list(
      list(
        "name" = "consortium",
        "display_name" = "Consortium",
        "recode" = list("na_replace" = "Not Annotated"),
        "truncate" = 15
      ),
      list(
        "name" = "year",
        "display_name" = "Year",
        "type" = "integer"
      )
    )
  )
  data1 <- dplyr::tribble(
    ~consortium,                 ~year, ~month,
    NA,                          2000L, NA,
    "c1",                        2001L, "January",
    "loooooooooooooooooooooong", 2001L, "January"
  )
  expected1 <- dplyr::tribble(
    ~Consortium,       ~Year,
    "Not Annotated",  2000L,
    "c1",              2001L,
    "looooooooooo...", 2001L
  )
  expect_equal(
    format_plot_data_with_config(data1, config1),
    expected1
  )
})

test_that("format_plot_data_with_config2", {

  config2 <- list(
    "drop_na" = T,
    "columns" = list(
      list(
        "name" = "consortium",
        "display_name" = "Consortium",
        "type" = "character",
        "truncate" = 15
      ),
      list(
        "name" = "year",
        "display_name" = "Year",
        "type" = "integer"
      )
    )
  )
  data2 <- dplyr::tribble(
    ~consortium,                 ~year, ~month,
    NA,                          2000L, NA,
    "c1",                        2001L, "January",
    "loooooooooooooooooooooong", 2001L, "January"
  )
  expected2 <- dplyr::tribble(
    ~Consortium,       ~Year,
    "c1",              2001L,
    "looooooooooo...", 2001L
  )
  expect_equal(
    format_plot_data_with_config(data2, config2),
    expected2
  )
})

test_that("format_plot_data_with_config3", {
  config3 <- list(
    "drop_na" = T,
    "columns" = list(
      list(
        "name" = "consortium",
        "display_name" = "Consortium"
      ),
      list(
        "name" = "year",
        "display_name" = "Year",
        "type" = "integer"
      )
    )
  )

  data3 <- dplyr::tibble(
    "consortium" = list(c("C1", "C2"), "C1", NA),
    "year" = 2020L
  )

  expected3 <- dplyr::tibble(
    "Consortium" = c("C1 | C2", "C1"),
    "Year" = 2020L
  )
  expect_equal(
    format_plot_data_with_config(data3, config3),
    expected3
  )

})

test_that("format_plot_data_with_config4", {
  config <- list(
    "unlist_columns" = list("consortium"),
    "columns" = list(
      list(
        "name" = "consortium",
        "display_name" = "Consortium"
      ),
      list(
        "name" = "year",
        "display_name" = "Year"
      )
    )
  )

  data <- dplyr::tibble(
    "consortium" = list(c("C1", "C2"), "C1", NA),
    "year" = c(2020L, 2019L, 2018L)
  )

  expected <- dplyr::tibble(
    "Consortium" = c("C1", "C2", "C1", NA),
    "Year" = c(2020L, 2020L, 2019L, 2018L)
  )
  expect_equal(
    format_plot_data_with_config(data, config),
    expected
  )

})

test_that("format_plot_data_with_config5", {
  tbl <- dplyr::tribble(
    ~col1,  ~col2,  ~year,
    "a",    "x",    2000L,
    "a",    "x",    2000L,
    "a",    "x",    2001L,
    "b",    "x",    2001L,
    "c",    NA,     NA
  )

  config <- list(
    "count_column" = list(
      "name" = "Count",
      "complete_columns" = list("col1", "year")
    ),
    "columns" = list(
      list(
        "name" = "col1",
        "display_name" = "Col1"
      ),
      list(
        "name" = "col2",
        "display_name" = "Col2"
      ),
      list(
        "name" = "year",
        "display_name" = "Year"
      )
    )
  )

  actual <- format_plot_data_with_config(tbl, config)
  expected <- dplyr::tribble(
    ~Col1,  ~Year,  ~Col2, ~Count,
    "a",    2000L,  "x",   2L,
    "a",    2001L,  "x",   1L,
    "b",    2000L,  NA,    0L,
    "b",    2001L,  "x",   1L,
    "c",    2000L,  NA,    0L,
    "c",    2001L,  NA,    0L
  ) %>%
    dplyr::mutate(
      "Col1" = forcats::as_factor(.data$Col1),
      "Year" = forcats::as_factor(.data$Year)
    )

  expect_equal(actual, expected)

})
