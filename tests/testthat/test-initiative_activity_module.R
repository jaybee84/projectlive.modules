
test_that("initiative_activity_module_ui", {
  expect_type(initiative_activity_module_ui("id"), "list")
})

test_that("initiative_activity_module_server_nf", {
  shiny::testServer(
    initiative_activity_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = nf_summary_snapshot_config %>%
        purrr::pluck("initiative_activity") %>%
        shiny::reactiveVal()
    ),
    {
      expect_type(plot_data(), "list")
      expect_type(output$plot, "character")
    }
  )
})

test_that("initiative_activity_module_server_csbc", {
  shiny::testServer(
    initiative_activity_module_server,
    args = list(
      "data" = shiny::reactiveVal(csbc_data),
      "config" = csbc_summary_snapshot_config %>%
        purrr::pluck("initiative_activity") %>%
        shiny::reactiveVal()
    ),
    {
      expect_type(plot_data(), "list")
      expect_type(output$plot, "character")
    }
  )
})
