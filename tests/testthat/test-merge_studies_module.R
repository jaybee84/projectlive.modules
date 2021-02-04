
test_that("merge_studies_module_ui", {
  expect_type(merge_studies_module_ui("id"), "list")
})

test_that("merge_studies_module_server", {
  shiny::testServer(
    merge_studies_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(nf_study_summary_config)
    ),
    {
      expect_type(merged_table(), "list")
      expect_type(study_table(), "list")
      expect_type(output$study_table, "character")

      session$setInputs("study_table_rows_selected" = 82)
      expect_type(selected_study_name(), "character")
      expect_type(output$study, "list")
      expect_type(filtered_merged_table(), "list")
      res <- session$getReturned()()
      expect_type(res$selected_study, "character")
      expect_named(
        res$tables,
        c('files', 'publications', 'studies', 'tools', 'merged')
      )

    }
  )
})

# test_that("merge_studies_module_server", {
#   shiny::testServer(
#     merge_studies_module_server,
#     args = list(
#       "data" = shiny::reactiveVal(csbc_data),
#       "config" = shiny::reactiveVal(csbc_study_summary_config)
#     ),
#     {
#       expect_type(merged_table(), "list")
#       expect_type(study_table(), "list")
#       expect_type(output$study_table, "character")
#
#       session$setInputs("study_table_rows_selected" = 82)
#       expect_type(selected_study_name(), "character")
#       expect_type(output$study, "list")
#       expect_type(filtered_merged_table(), "list")
#       res <- session$getReturned()()
#       expect_type(res$selected_study, "character")
#       expect_named(
#         res$tables,
#         c('files', 'publications', 'studies', 'tools', 'merged')
#       )
#
#     }
#   )
# })
