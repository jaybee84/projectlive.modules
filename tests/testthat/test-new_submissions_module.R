test_that("new_submissions_module_ui", {
  expect_type(new_submissions_module_ui("id"), "list")
})

test_that("new_submissions_module_server", {
  shiny::testServer(
    new_submissions_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(nf_new_submissions_config)
    ),
    {
      session$setInputs(
        "new_files_day_choice" = 5
      )
      expect_type(output$header_text, "character")
      expect_type(minimum_date(), "double")
      expect_type(new_files_table(), "list")
      expect_named(
        new_files_table(),
        c(
          'File Name',
          'Date',
          'Study Name',
          'Study Leads',
          'Parent ID',
          'Resource Type',
          'Assay'
        )
      )
      expect_type(output$new_files_dt, "character")
    }
  )
})
