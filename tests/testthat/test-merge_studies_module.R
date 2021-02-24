
test_that("merge_studies_module_ui", {
  expect_type(merge_studies_module_ui("id"), "list")
})

test_that("nf_merge_studies_module_server1", {
  shiny::testServer(
    merge_studies_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(nf_study_summary_config)
    ),
    {
      expect_type(filter_choices(), "character")
      expect_type(output$filter_ui, "list")
      session$setInputs("filter_value" = "All")
      expect_type(filtered_table(), "list")
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

test_that("nf_merge_studies_module_server2", {
  shiny::testServer(
    merge_studies_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(nf_study_summary_config)
    ),
    {
      expect_type(filter_choices(), "character")
      expect_type(output$filter_ui, "list")
      session$setInputs("filter_value" = filter_choices()[[1]])
      expect_type(filtered_table(), "list")
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


test_that("nf_gff_merge_studies_module_server", {
  shiny::testServer(
    merge_studies_module_server,
    args = list(
      "data" = shiny::reactive(nf_gff_data),
      "config" = shiny::reactive(nf_study_summary_config)
    ),
    {
      expect_type(output$filter_ui, "list")
      session$setInputs("filter_value" = "All")
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
      expect_type(output$filter_ui, "list")
      session$setInputs("filter_value" = "All")
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
