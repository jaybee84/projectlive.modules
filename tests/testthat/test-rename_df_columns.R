test_that("rename_df_columns_with_config", {
  tbl1 <- dplyr::tibble(
    "col1" = character(), "col2" = character(), "colNum3" = character()
  )
  config1 <- list(
    "columns" = list(
      "col1" = list("name" = "col1", "display_name" = "Column1"),
      "col2" = list("name" = "col2"),
      "col3" = list("name" = "colNum3")
    )
  )
  res1 <- rename_df_columns_with_config(tbl1, config1)
  expect_equal(
    res1,
    dplyr::tibble(
      "Column1" = character(), "Col2" = character(), "Colnum3" = character()
    )
  )


  config2 <- list(
    "columns" = list(
      list("name" = "col1", "display_name" = "Column1"),
      list("name" = "col2", "display_name" = "Column2"),
      list("name" = "colNum3", "display_name" = "Column3")
    )
  )
  res2 <- rename_df_columns_with_config(tbl1, config2)
  expect_equal(
    res2,
    dplyr::tibble(
      "Column1" = character(), "Column2" = character(), "Column3" = character()
    )
  )
})
