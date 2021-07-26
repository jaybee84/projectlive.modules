test_that("create_oauth_list", {
  result1 <- create_oauth_list(config = list(
    "client_id" = "1",
    "client_secret" = "XXX",
    "app_url" = " http://127.0.0.1:8100"
  ))
  expect_type(result1, "list")
  expect_named(result1, c("config", "app", "endpoint", "scope"))
  expect_type(result1$config, "list")
  expect_type(result1$app, "list")
  expect_type(result1$endpoint, "list")
  expect_type(result1$scope, "character")
})
