
test_that("summary_snapshot_module_ui", {
  expect_type(summary_snapshot_module_ui("id"), "list")
})

test_that("summary_snapshot_module_server", {
  shiny::testServer(
    summary_snapshot_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(get_nf_summary_snapshot_config())
    ),
    {
      expect_type(output$header_text, "character")
      expect_type(output$box1, "list")
      expect_type(output$box2, "list")
      expect_type(output$box3, "list")
      expect_type(output$box4, "list")
    }
  )
})

test_that("summary_snapshot_module_server_error", {
  shiny::testServer(
    summary_snapshot_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(list())
    ),
    {
      expect_error(config_is_valid())
    }
  )
})
