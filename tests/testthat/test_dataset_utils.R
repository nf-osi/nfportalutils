# Create a basic draft dataset from some files at version 1; all files have a latest version 2 
# Returns dataset id only
create_dataset_fixture <- function(instance = 1) {
  NF_test <- "syn26462036"
  items <- c("syn51239179",
             "syn51239178",
             "syn51239177")
  dataset <- new_dataset(name = paste0("test_fixture_dataset_", instance), parent = NF_test, items = items, item_version = 1L, dry_run = FALSE)
  dataset_id <- dataset$properties$id
  dataset_id
}


test_that("Creating dataset with `new_dataset` works as expected when given valid parameters, defaulting to current item versions", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  NF_test <- "syn26462036"
  # Note that files are all version 2 on Synapse
  items <- c("syn51239179",
             "syn51239178",
             "syn51239177")
  dataset <- new_dataset(name = "test_dataset", parent = NF_test, items = items, dry_run = FALSE)
  expected_items_in_dataset <- list(
    list(entityId = "syn51239179", versionNumber = 2L),
    list(entityId = "syn51239178", versionNumber = 2L),
    list(entityId = "syn51239177", versionNumber = 2L)) 
  testthat::expect_equal(expected_items_in_dataset, dataset$properties$datasetItems)
  .syn$delete(dataset)
})


test_that("Creating dataset with `new_dataset` works as expected when given valid parameters and a specific item version is specified", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  NF_test <- "syn26462036"
  items <- c("syn51239179",
             "syn51239178",
             "syn51239177")
  dataset <- new_dataset(name = "test_dataset", parent = NF_test, items = items, item_version = 1L, dry_run = FALSE)
  expected_items_in_dataset <- list(
    list(entityId = "syn51239179", versionNumber = 1L),
    list(entityId = "syn51239178", versionNumber = 1L),
    list(entityId = "syn51239177", versionNumber = 1L))
  testthat::expect_equal(expected_items_in_dataset, dataset$properties$datasetItems)
  .syn$delete(dataset)
})


# When providing an item not allowed to be a dataset item (a table or folder), the Synapse error will be something like
# ```
# Error: synapseclient.core.exceptions.SynapseHTTPError: 400 Client Error:
# Currently, only files can be included in a dataset. syn27242487 is 'org.sagebionetworks.repo.model.table.TableEntity'
# ```
test_that("Creating dataset with `new_dataset` will fail when trying to include a non-valid item (a table)", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  NF_test <- "syn26462036"
  items <- c("syn51239179",
             "syn51239178",
             "syn27242487") # This is a table
  testthat::expect_error(dataset <- new_dataset(name = "test_dataset", parent = NF_test, items = items, dry_run = FALSE))
})


test_that("Updating a dataset to make a subset of files reference the latest version works", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  
  dataset_id <- create_dataset_fixture()
  items_to_update <- c("syn51239178", "syn51239177") # both should be updated to Version 2
  updated <- use_latest_in_collection(collection_id = dataset_id, items = items_to_update)
  expected_updated_items <- list(
    list(entityId = "syn51239179", versionNumber = 1L),
    list(entityId = "syn51239178", versionNumber = 2L),
    list(entityId = "syn51239177", versionNumber = 2L))
  testthat::expect_identical(updated$items, expected_updated_items)
  .syn$delete(dataset_id)
})


test_that("Updating a dataset to make _all_ files reference the latest version works", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  
  dataset_id <- create_dataset_fixture()
  expected_updated_items <- list(
    list(entityId = "syn51239179", versionNumber = 2L),
    list(entityId = "syn51239178", versionNumber = 2L),
    list(entityId = "syn51239177", versionNumber = 2L))
  updated <- use_latest_in_collection(collection_id = dataset_id, items = "all")
  testthat::expect_identical(updated$items, expected_updated_items)
  .syn$delete(dataset_id)
})


# Dataset collections ---------------------------------------------------------#

test_that("Updating a dataset collection to make a subset of datasets reference the latest version works", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  
  dataset_collection_id <- "syn51809938"
  dataset_item_to_update <- "syn51809898"
  .syn$create_snapshot_version(dataset_item_to_update)
  new_version <- .syn$get(dataset_item_to_update, downloadFile = FALSE)$properties$versionNumber
  DC <- use_latest_in_collection(collection_id = dataset_collection_id, items = dataset_item_to_update)
  updated_item <- Filter(function(item) item$entityId == dataset_item_to_update, DC$items)
  testthat::expect_equal(updated_item[[1]]$versionNumber, new_version)
})


test_that("Adding new dataset to dataset collection works", {

  skip_if_no_synapseclient()
  skip_if_no_token()
  
  dataset_collection_id <- "syn51809938"
  coll_state <- coll <- .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{dataset_collection_id}"))
  one_more_item <- create_dataset_fixture()
  new_coll_state <- add_to_collection(collection_id = dataset_collection_id, items = one_more_item)
  testthat::expect_equal(length(new_coll_state$items), length(coll_state$items) + 1L)
  # cleanup: set collection to previous items state
  new_coll_state$items <- coll_state$items
  .syn$restPUT(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{dataset_collection_id}"), body = jsonlite::toJSON(new_coll_state, auto_unbox = TRUE))
  # delete dataset
  .syn$delete(one_more_item)
})


test_that("Adding non-datasets to dataset collection gives expected handling and warning", {
  
  skip_if_no_synapseclient()
  skip_if_no_token()
  
  dataset_collection_id <- "syn51809938"
  bad_items <- "syn51106349" # a folder
  testthat::expect_warning(add_to_collection(collection_id = dataset_collection_id, items = bad_items, check_items = TRUE),
                           regexp = paste("No qualifying items to add. No updates applied."))
})

