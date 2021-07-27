test_that("select_df_columns_with_config", {
  tbl1 <- dplyr::tibble(
    "col1" = character(),
    "col2" = character(),
    "col3" = character(),
    "col4" = character()
  )
  config1 <- list(
    "columns" = list(
      list("name" = "col1", "display_name" = "Column1"),
      list("name" = "col2"),
      list("name" = "col3")
    )
  )
  res1 <- select_df_columns_with_config(tbl1, config1)
  expect_equal(
    res1,
    dplyr::tibble(
      "col1" = character(), "col2" = character(), "col3" = character()
    )
  )
})
