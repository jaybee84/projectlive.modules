test_that("concatenate_list_columns", {
  tbl1 <- dplyr::tibble(
    "cola" = list(c("a", "b"), "a", c("a", "c")),
    "colb" = c("a", "b", "c"),
    "colc" = c("a", "b", NA),
    "cold" = list(c("a", "b"), "a", NA),
    "colf" = list(c(NA, NA), NA, c("c", "a", NA))
  )
  col1 <- "cola"
  col2 <- "colb"
  col3 <- "colc"
  col4 <- "cold"
  col5 <- "colf"

  res1 <- concatenate_list_columns(tbl1, col1)
  expect_equal(
    res1,
    dplyr::tibble(
      "cola" = c("a | b", "a", "a | c"),
      "colb" = c("a", "b", "c"),
      "colc" = c("a", "b", NA),
      "cold" = list(c("a", "b"), "a", NA),
      "colf" = list(c(NA, NA), NA, c("c", "a", NA))
    )
  )

  res2 <- concatenate_list_columns(tbl1, col2)
  expect_equal(res2, tbl1)

  res3 <- concatenate_list_columns(tbl1, col3)
  expect_equal(res3, tbl1)

  res4 <- concatenate_list_columns(tbl1, col4)
  expect_equal(
    res4,
    dplyr::tibble(
      "cola" = list(c("a", "b"), "a", c("a", "c")),
      "colb" = c("a", "b", "c"),
      "colc" = c("a", "b", NA),
      "cold" = c("a | b", "a", NA),
      "colf" = list(c(NA, NA), NA, c("c", "a", NA))
    )
  )

  res5 <- concatenate_list_columns(tbl1, col5)
  expect_equal(
    res5,
    dplyr::tibble(
      "cola" = list(c("a", "b"), "a", c("a", "c")),
      "colb" = c("a", "b", "c"),
      "colc" = c("a", "b", NA),
      "cold" = list(c("a", "b"), "a", NA),
      "colf" = c(NA, NA, "a | c"),
    )
  )

})
