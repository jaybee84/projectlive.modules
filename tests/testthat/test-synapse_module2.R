test_that("synapse_module_ui2", {
  expect_type(synapse_module_ui2("id"), "list")
})

synapseclient <- reticulate::import("synapseclient", delay_load = TRUE)
syn <- synapseclient$Synapse()
invisible(syn$login())

test_that("synapse_module_server2", {
  shiny::testServer(
    synapse_module_server2,
    args = list(
      syn = syn,
      config = shiny::reactive(get_nf_synapse_config())
    ),
    {
      session$setInputs("selected_group" = "NTAP")
      expect_type(current_user_synapse_id(), "character")
      expect_type(output$about, "list")
      expect_type(groups_allowed(), "character")
      expect_equal(
        groups_allowed(),
        # TODO: remove
        c("CDMRP NFRP", "CTF", "GFF", "NCI DHART SPORE", "NFRI", "NTAP")
      )
      expect_type(output$group_selection_ui, "list")
      expect_type(output$group, "character")
      expect_type(tables(), "list")
      expect_named(tables(), c('files', 'publications', 'studies', 'tools'))
      expect_type(filtered_tables(), "list")
      expect_named(filtered_tables(), c('files', 'publications', 'studies', 'tools'))
      expect_type(data(), "list")
      expect_named(data(), c("tables", "selected_group"))
    }
  )
})
