test_that("synapse_module_ui", {
  expect_type(synapse_module_ui("id"), "list")
})


synapseclient <- reticulate::import("synapseclient", delay_load = TRUE)
syn <- synapseclient$Synapse()
invisible(syn$login())


test_that("synapse_module_server", {
  shiny::testServer(
    synapse_module_server,
    args = list(
      syn = syn,
      config = shiny::reactive(get_csbc_synapse_config())
    ),
    {
      expect_true(T)
      expect_type(output$about, "list")
      expect_type(output$txt, "character")
      expect_type(data(), "list")
      expect_named(data(), "tables")
      tables <- data()$tables
      expect_type(tables, "list")
      expect_named(
        tables, c('files', 'datasets', 'publications', 'studies', 'tools')
      )
    }
  )
})
