
test_that("publication_status_module_ui", {
  expect_type(summary_snapshot_module_ui("id"), "list")
})

test_that("nf_publication_status_module_ui", {
  shiny::testServer(
    summary_snapshot_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(nf_publication_status_config)
    ),
    {
      expect_type(output$header_text, "character")
    }
  )
})
