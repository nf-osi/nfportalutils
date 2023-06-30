test_that("Creating dataset with `new_dataset` works as expected when given valid parameters, defaulting to current item versions", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  NF_test <- "syn26462036"
  # Note that files are all version 2 on Synapse
  items <- c("syn51239179",
             "syn51239178",
             "syn51239177")
  dataset <- new_dataset(name = "test_dataset", parent = NF_test, items = items, dry_run = FALSE)
  .syn$delete(dataset)
  expected_items_in_dataset <- list(list(entityId = "syn51239179", versionNumber = 2L),
                                    list(entityId = "syn51239178", versionNumber = 2L),
                                    list(entityId = "syn51239177", versionNumber = 2L)) 
  testthat::expect_equal(expected_items_in_dataset, dataset$properties$datasetItems)
  
})

test_that("Creating dataset with `new_dataset` works as expected when given valid parameters and a specific item version is specified", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  NF_test <- "syn26462036"
  items <- c("syn51239179",
             "syn51239178",
             "syn51239177")
  dataset <- new_dataset(name = "test_dataset", parent = NF_test, items = items, item_version = 1L, dry_run = FALSE)
  .syn$delete(dataset)
  expected_items_in_dataset <- list(list(entityId = "syn51239179", versionNumber = 1L),
                                    list(entityId = "syn51239178", versionNumber = 1L),
                                    list(entityId = "syn51239177", versionNumber = 1L))
  testthat::expect_equal(expected_items_in_dataset, dataset$properties$datasetItems)
  
})

# When providing an item not allowed to be a dataset item (a table or folder), the Synapse error will be something like
# ```
# Error: synapseclient.core.exceptions.SynapseHTTPError: 400 Client Error:
# Currently, only files can be included in a dataset. syn27242487 is 'org.sagebionetworks.repo.model.table.TableEntity'
# ```
# This is a good, informative error
test_that("Creating dataset with `new_dataset` will fail when trying to include a non-vaid item (specifically, a table)", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  NF_test <- "syn26462036"
  items <- c("syn51239179",
             "syn51239178",
             "syn27242487") # This is a table
  testthat::expect_error(dataset <- new_dataset(name = "test_dataset", parent = NF_test, items = items, dry_run = FALSE))
})


