
test_that("study_summary_module_ui", {
  expect_type(study_summary_module_ui("id"), "list")
})

test_that("study_summary_module_server", {
  shiny::testServer(
    study_summary_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(nf_study_summary_config)
    ),
    {
      expect_type(data2(), "list")
      expect_named(data2(), c("tables", "selected_group"))
      tables <- data2()$tables
      expect_type(tables, "list")
      expect_named(
        tables,
        c("files", "publications", "studies", "tools", "merged")
      )
      expect_type(tables$merged, "list")
      expect_type(output$header_text, "character")
      expect_type(output$study_table, "character")

      session$setInputs("study_table_rows_selected" = 82)
      expect_type(selected_study_name(), "character")
      expect_type(output$study, "list")
      expect_type(filtered_merged_table(), "list")

      expect_type(output$annotation_activity, "character")
      expect_type(output$data_focus_selection_ui, "list")
      expect_type(output$data_focus_plot, "character")
      expect_type(output$study_timeline_plot, "character")
      expect_type(output$publication_status, "character")
      expect_type(output$study_details, "character")
    }
  )
})
