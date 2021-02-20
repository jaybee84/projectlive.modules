test_that("select_df_columns_with_config", {
  tbl1 <- dplyr::tibble("col1" = c(), "col2" = c(), "col3" = c(), "col4" = c())
  config1 <- list(
    "columns" = list(
      list("name" = "col1", "display_name" = "Column1", "type" = "x"),
      list("name" = "col2", "type" = "x"),
      list("name" = "col3", "type" = "x")
    )
  )
  res1 <- select_df_columns_with_config(tbl1, config1)
  expect_equal(
    res1,
    dplyr::tibble("col1" = c(), "col2" = c(), "col3" = c())
  )
})
