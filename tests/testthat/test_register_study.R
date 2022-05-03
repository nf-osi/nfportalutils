# This uses a test table (syn27353709) mirroring current schema of production study table
# If the study table schema changes we need to update the register study function 

test_that("can't register a study with missing meta (e.g. institution)", {
  skip_if_no_synapseclient()
  skip_if_no_pandas()
  skip_if_no_login()
  testthat::expect_error(
    register_study(name = "Test study", 
                   project_id = "syn12345678", 
                   abstract = "Our project investigates...", 
                   lead = "Study Lead", 
                   # institution = "Institute of High Research", 
                   focus = "Neurofibromatosis type 1", 
                   manifestation = "Cutaneous Neurofibroma", 
                   funder = "NTAP",
                   initiative = "Special Initiative",
                   fileview_id = "syn11223344", 
                   study_table_id = "syn27353709"),
    regexp = 'argument "institution" is missing'
  )
})

test_that("can register a study with non-list attributes (simplest expected meta)", {
  skip_if_no_synapseclient()
  skip_if_no_pandas()
  skip_if_no_login()
  testthat::expect_s3_class(
    register_study(name = "Test study", 
                   project_id = "syn12345678", 
                   abstract = "Our project investigates...", 
                   lead = "Study Lead", 
                   institution = "Institute of High Research", 
                   focus = "Neurofibromatosis type 1", 
                   manifestation = "Cutaneous Neurofibroma", 
                   funder = "NTAP",
                   initiative = "Special Initiative",
                   fileview_id = "syn11223344", 
                   study_table_id = "syn27353709"),
    "synapseclient.table.CsvFileTable"
  )
})

# Note that this test is actually incomplete;
# we need to pull the row and check that it *does* contain fields as a list
test_that("can register a study with list attributes", {
  skip_if_no_synapseclient()
  skip_if_no_pandas()
  skip_if_no_login()
  testthat::expect_s3_class(
    register_study(name = "Test study", 
                   project_id = "syn12345678", 
                   abstract = "Our project investigates...", 
                   initiative = "Special Initiative", 
                   lead = "Study Lead 1, Study Lead 2", 
                   institution = "Institute of High Research; University of California, San Diego", 
                   focus = "Neurofibromatosis type 1, Neurofibromatosis type 2", 
                   manifestation = "Schwannoma, Meningioma",
                   funder = "NTAP, Collaborative Funder",
                   fileview_id = "syn11223344", 
                   grant_doi = "https://doi.org/10.1109/123.456, https://doi.org/10.1109/999.999",
                   study_table_id = "syn27353709"),
    "synapseclient.table.CsvFileTable"
  )
})
