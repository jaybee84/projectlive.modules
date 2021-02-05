
test_that("merge_studies_module_ui", {
  expect_type(merge_studies_module_ui("id"), "list")
})

test_that("nf_merge_studies_module_server", {
  shiny::testServer(
    merge_studies_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(nf_study_summary_config)
    ),
    {
      expect_type(study_table(), "list")
      expect_type(output$study_table, "character")
      session$setInputs("study_table_rows_selected" = 3)
      expect_type(selected_study_name(), "character")
      expect_type(output$study, "list")
      res <- session$getReturned()()
      expect_type(res$selected_study, "character")

    }
  )
})

test_that("csbc_merge_studies_module_server", {
  shiny::testServer(
    merge_studies_module_server,
    args = list(
      "data" = shiny::reactiveVal(csbc_data),
      "config" = shiny::reactiveVal(csbc_study_summary_config)
    ),
    {
      expect_type(study_table(), "list")
      expect_type(output$study_table, "character")
      session$setInputs("study_table_rows_selected" = 34)
      expect_type(selected_study_name(), "character")
      expect_equal(
        selected_study_name(),
        "H Lee Moffitt Cancer Center and Research Institute"
      )
      expect_type(output$study, "list")
      res <- session$getReturned()()
      expect_type(res$selected_study, "character")
      expect_equal(
        res$selected_study,
        "H Lee Moffitt Cancer Center and Research Institute"
      )
    }
  )
})
