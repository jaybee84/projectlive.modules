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
    "unnest_cols" = "consortium",
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
