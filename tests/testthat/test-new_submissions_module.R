test_that("new_submissions_module_ui", {
  expect_type(new_submissions_module_ui("id"), "list")
})

test_that("new_submissions_module_server", {
  shiny::testServer(
    new_submissions_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(nf_new_submissions_config)
    ),
    {
      expect_type(output$header_text, "character")
    }
  )
})
