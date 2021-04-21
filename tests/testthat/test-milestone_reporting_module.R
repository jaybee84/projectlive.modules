
test_that("milestone_reporting_module_ui", {
  expect_type(milestone_reporting_module_ui("id"), "list")
})

test_that("milestone_reporting_module_server", {
  shiny::testServer(
    milestone_reporting_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(
        nf_study_summary_config$milestone_reporting_plot
      )
    ),
    {
      expect_type(files_tbl(), "list")
      expect_type(id_tbl(), "list")
      session$setInputs("days_choice" = 1000)

      expect_type(dt_tbl1(), "list")
      expect_type(output$dt1, "character")
      session$setInputs("dt1_rows_selected" = 2)
      expect_type(dt_row1(), "list")
      expect_type(filtered_id_tbl1(), "list")
      expect_type(filtered_files_tbl1(), "list")
      expect_type(merged_tbl1(), "list")
      expect_type(plot_obj1(), "list")

      expect_type(milestone_choices(), "integer")
      expect_type(output$milestone_choice_ui, "list")
      session$setInputs("milestone_choice" = 2)
      expect_type(filtered_id_tbl2(), "list")
      expect_type(filtered_files_tbl2(), "list")
      expect_type(merged_tbl2(), "list")
      expect_type(plot_obj2(), "list")
    }
  )
})
