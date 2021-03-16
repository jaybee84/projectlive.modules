test_that("mod_integration_timeline_ui", {
  expect_type(mod_integration_timeline_ui("id"), "list")
})

test_that("mod_integration_timeline_server", {
  shiny::testServer(
    mod_integration_timeline_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(NULL)
    ),
    {
      expect_type(id_tbl(), "list")
      expect_type(dt_tbl(), "list")
      expect_type(output$dt, "character")
      session$setInputs("dt_rows_selected" = 1)
      print(dt_row())
      print(filtered_id_tbl())
      # session$setInputs("days_selected" = 100000)
      # print(dt_tbl(), n = 100)
      # print(row())
      # # print(files())
      # print(merged_table())
    }
  )
})
