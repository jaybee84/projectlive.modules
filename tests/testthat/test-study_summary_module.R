
test_that("study_summary_module_ui", {
  expect_type(study_summary_module_ui("id"), "list")
})

test_that("nf_study_summary_module_server", {
  shiny::testServer(
    study_summary_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(get_nf_study_summary_config())
    ),
    {

      expect_type(output$header_text, "character")
      session$setInputs("study_summary-filter_value" = "All")
      session$setInputs("study_summary-study_table_rows_selected" = 9)
      expect_type(output$study_summary, "character")
      expect_type(output$data_focus_plot, "character")
    }
  )
})

test_that("nf_gff_study_summary_module_server", {
  shiny::testServer(
    study_summary_module_server,
    args = list(
      "data" = shiny::reactive(nf_gff_data),
      "config" = shiny::reactive(get_nf_study_summary_config())
    ),
    {
      expect_type(output$header_text, "character")
      session$setInputs("study_summary-filter_value" = "All")
      session$setInputs("study_summary-study_table_rows_selected" = 3)
      expect_type(output$study_summary, "character")
      expect_error(
        output$data_focus_plot,
        "The investigators have not uploaded data for this study yet. Please check back later."
      )

    }
  )
})


test_that("csbc_study_summary_module_server", {
  shiny::testServer(
    study_summary_module_server,
    args = list(
      "data" = shiny::reactiveVal(csbc_data),
      "config" = shiny::reactiveVal(get_csbc_study_summary_config())
    ),
    {
      expect_type(output$header_text, "character")
      session$setInputs("study_summary-filter_value" = "All")
      session$setInputs("study_summary-study_table_rows_selected" = 34)
      expect_equal(
        filtered_data()$selected_study,
        "H Lee Moffitt Cancer Center and Research Institute"
      )

      expect_type(output$study_summary, "character")
      expect_type(output$data_focus_plot, "character")
    }
  )
})
