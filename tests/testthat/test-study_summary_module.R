
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
      expect_type(output$header_text, "character")
      session$setInputs("merge_studies-study_table_rows_selected" = 82)

      expect_type(output$study_summary, "character")
      expect_type(output$study_timeline_plot, "character")
      expect_type(output$data_focus_plot, "character")
      expect_type(output$annotation_activity_plot, "character")
      expect_type(output$publication_status_plot, "character")

    }
  )
})
