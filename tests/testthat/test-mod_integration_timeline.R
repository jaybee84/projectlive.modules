test_that("mod_integration_timeline_ui", {
  expect_type(mod_integration_timeline_ui("id"), "list")
})

test_that("mod_integration_timeline_server", {
  shiny::testServer(
    mod_integration_timeline_server,
    args = list(
      "data" = shiny::reactiveVal(NULL),
      "config" = shiny::reactiveVal(NULL)
    ),
    {
      expect_true(T)

    }
  )
})
