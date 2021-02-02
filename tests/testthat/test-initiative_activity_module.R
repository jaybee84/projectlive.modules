
test_that("initiative_activity_module_ui", {
  expect_type(initiative_activity_module_ui("id"), "list")
})

test_that("initiative_activity_module_server", {
  shiny::testServer(
    initiative_activity_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = nf_summary_snapshot_config %>%
        purrr::pluck("initiative_activity") %>%
        shiny::reactiveVal()
    ),
    {
      expect_type(data(), "list")
      expect_type(output$plot, "character")
    }
  )
})
