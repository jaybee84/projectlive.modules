test_that("filter_column_for_n_most_values", {
  tbl1 <- dplyr::tibble(
    "col1" = c("a", "a", "b", "b", "c"),
    "col2" = c("b", "b", "a", "a", "c"),
    "col3" = c(1L, 1L, 2L, 2L, 3L),
    "col4" = factor(c(1L, 1L, 2L, 2L, 3L))
  )

  actual1 <- filter_column_for_n_most_values(tbl1, "col1", 1)
  expected1 <- tbl1 <- dplyr::tibble(
    "col1" = c("a", "a"),
    "col2" = c("b", "b"),
    "col3" = c(1L, 1L),
    "col4" = factor(c(1L, 1L), levels = c(1L, 2L, 3L))
  )
  expect_equal(actual1, expected1)
  #
  # actual2 <- filter_column_for_n_most_values(tbl1, "col1", 2)
  # expected2 <- dplyr::tibble(
  #   "col1" = c("a", "a", "b", "b"),
  #   "col2" = c("b", "b", "a", "a"),
  #   "col3" = c(1L, 1L, 2L, 2L),
  #   "col4" = factor(1L, 1L, 2L, 2L)
  # )
  # expect_equal(actual2, expected2)
})
