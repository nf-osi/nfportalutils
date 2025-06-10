
# -- Entity type checks------------- --------------------------------------------#

#' Check whether entity is dataset
#'
#' @keywords internal
is_dataset <- function(id) {
  tryCatch({
    entity <- .syn$get(id, downloadFile = FALSE)
    entity$properties$concreteType == "org.sagebionetworks.repo.model.table.Dataset"
  },
  error = function(e) FALSE)
}

#' Check whether entity is dataset collection
#'
#' @keywords internal
is_dataset_collection <- function(id) {
  tryCatch({
    entity <- .syn$get(id, downloadFile = FALSE)
    entity$properties$concreteType == "org.sagebionetworks.repo.model.table.DatasetCollection"
  },
  error = function(e) FALSE)
}


#' Which collection type
#'
#' Checks for a valid collection type or returns error
#'
#' @keywords internal
which_coll_type <- function(coll) {
  coll_type <- c("dataset", "dataset collection")[c(is_dataset(coll), is_dataset_collection(coll))]
  if(length(coll_type)) coll_type else stop("Entity is not a dataset or dataset collection.")
}

#' Check whether entity is file
#'
#' @keywords internal
is_file <- function(id) {
  tryCatch({
    entity <- .syn$get(id, downloadFile = FALSE)
    entity$properties$concreteType == "org.sagebionetworks.repo.model.FileEntity"
  },
  error = function(e) FALSE)
}

# -- Metadata schema-based validation ------------------------------------------------#

#' Validate metadata of items in a collection
#' 
#' This is usually used with a dataset collection.
#' For each item in the collection, check first that it is bound to the expected schema.
#' Then check that the item's annotations validate against the schema, using Synapse's native validation services. 
#' 
#' The result is a list of ids for `bound_schema_error` and for `validation_error`. 
#' 
#' @param collection_id Collection id.
#' @param schema_id Id of schema that items are expected to be bound to, e.g. "org.synapse.nf-portaldataset".
validate_collection_items <- function(collection_id, schema_id) {
  coll <- .syn$get("syn50913342")
  items <- coll$properties$datasetItems
  if(!length(items)) stop("Collection has no items.")
  item_ids <- sapply(items, `[[`, "entityId")
  results <- list()
  results$bound_schema_error <- list()
  results$validation_error <- list()
  for (id in item_ids) {
    bound_schema <- tryCatch(.syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{id}/schema/binding")),
                             error = function(e) NULL)
    if(is.null(bound_schema)) {
      results$bound_schema_error[[id]] <- "No schema bound"
    } else {
      bound_schema_id <- bound_schema$jsonSchemaVersionInfo$`$id`
      if(bound_schema_id != schema_id) { 
        results$bound_schema_error[[id]] <- "Wrong schema bound"
      } else {
        entity_val_result <- .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{id}/schema/validation"))
        if(entity_val_result$isValid == FALSE) {
          results$validation_error[[id]] <- entity_val_result
        }
      }
    }
  }
  results
}

