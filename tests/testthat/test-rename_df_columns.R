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
      list("name" = "col1", "display_name" = "Column1", "type" = "x"),
      list("name" = "col2", "type" = "x", "display_name" = "Column2"),
      list("name" = "colNum3", "type" = "x", "display_name" = "Column3")
    )
  )
  res1 <- rename_df_columns_with_config(tbl1, config1)
  res2 <- rename_df_columns_with_config(tbl1, config2)
  expect_equal(
    res1,
    dplyr::tibble("Column1" = c(), "Col2" = c(), "Colnum3" = c())
  )
  expect_equal(
    res2,
    dplyr::tibble("Column1" = c(), "Column2" = c(), "Column3" = c())
  )
})
