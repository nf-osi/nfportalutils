# Utilities for binding schema and schema-based validation

#' Wrapper for JSON schema binding
#'
#' Binds a JSON schema to a Synapse entity. If a schema is already bound to the entity,
#' running this function again will replace the existing schema binding with the new one.
#'
#' See https://help.synapse.org/docs/JSON-Schemas.3107291536.html
#'
#' @param id Id of entity to which schema will be bound
#' @param schema_id Schema id as registered on Synapse.
#' Examples for "latest" or explicitly versioned schemas: "org.synapse.nf-portalstudy", "org.synapse.nf-rnaseqtemplate-10.2.0".
#' @param derived_annotations Whether to enabled derived annotations.
#' Default `FALSE` as this is the API default.
#' @export
#' @examples
#' \dontrun{
#' # Bind a dataset schema to a folder
#' bind_schema(id = "syn12345678", schema_id = "org.synapse.nf-rnaseqtemplate-10.2.0")
#'
#' # Bind with derived annotations enabled
#' bind_schema(id = "syn12345678",
#'             schema_id = "org.synapse.nf-protocol",
#'             derived_annotations = TRUE)
#'
#' # Replace an existing schema binding with a new one
#' bind_schema(id = "syn12345678", schema_id = "org.synapse.nf-rnaseqtemplate-11.0.0")
#' }
bind_schema <- function(id, schema_id, derived_annotations = FALSE) {

  .check_login()

  bind_schema_request <- jsonlite::toJSON(list(entityId = id,
                                               `schema$id` = schema_id,
                                               enableDerivedAnnotations = derived_annotations),
                                          auto_unbox = TRUE)
  binding_uri <- glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{id}/schema/binding")
  try(.syn$restPUT(binding_uri, bind_schema_request))
}


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

#' Validate a **schema-bound** entity
#'
#' This will error for unbound entities.
#'
#' @param id Entity id.
#' @export
validate_bound_entity <- function(id) {
  .check_login()
  .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{id}/schema/validation"))
}

#' Validate multiple entities in parallel
#'
#' Helper function to validate multiple entities against their bound schemas in parallel.
#'
#' @param entity_ids Vector of entity IDs to validate.
#' @param mc.cores Number of cores to use for parallel validation.
#' Defaults to `min(parallel::detectCores() / 2, 4)` to be conservative.
#' Set to 1 to disable parallel processing.
#' @return Named list of validation results, where names are entity IDs.
#' @keywords internal
validate_entities_parallel <- function(entity_ids, mc.cores = NULL) {

  if(length(entity_ids) == 0) {
    return(list())
  }

  # Determine number of cores to use
  if(is.null(mc.cores)) {
    # Conservative default: use half of available cores, max 4
    mc.cores <- min(max(1, floor(parallel::detectCores() / 2)), 4)
  }

  message("Validating ", length(entity_ids), " entities with ", mc.cores, " core(s)...")

  # Validate each entity in parallel
  validation_results <- parallel::mclapply(entity_ids, function(entity_id) {
    tryCatch({
      validate_bound_entity(entity_id)
    }, error = function(e) {
      list(isValid = NA, error = as.character(e))
    })
  }, mc.cores = mc.cores)

  names(validation_results) <- entity_ids
  return(validation_results)
}

#' Validate dataset folder
#'
#' Convenience function to validate a dataset folder.
#' This will use the schema bound to the dataset for validation.
#' If no schema is bound, this will give an error.
#' Files inherit the schema binding from the parent folder, so only the folder binding is checked.
#'
#' @param id Synapse id of a dataset folder
#' @param fileview Fileview to use for resolving folder items (required).
#' Must have the `path` column enabled to support recursive file discovery.
#' @param mc.cores Number of cores to use for parallel validation.
#' Defaults to `min(parallel::detectCores() / 2, 4)` to be conservative.
#' Set to 1 to disable parallel processing.
#' @return A list with `validation_error` component containing any validation errors found for files.
#' @export
validate_dataset_folder <- function(id, fileview, mc.cores = NULL) {

  .check_login()

  if(missing(fileview) || is.null(fileview)) {
    stop("A fileview is required to query files in the dataset folder.")
  }

  # Get bound schema for the folder
  bound_schema <- tryCatch(
    .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{id}/schema/binding")),
    error = function(e) NULL
  )

  if(is.null(bound_schema)) {
    stop("No schema bound to dataset folder. Please bind a schema first using bind_schema().")
  }

  # Get the path for the folder to use in query
  path <- get_path(id)

  # Query fileview for all files within this folder path (recursive)
  query <- glue::glue("SELECT id, name FROM {fileview} WHERE path LIKE '{path}%' AND type = 'file'")
  result <- .syn$tableQuery(query)$asDataFrame()

  if(nrow(result) == 0) {
    warning("Dataset folder has no files.")
    return(list(validation_error = list()))
  }

  file_ids <- result$id
  message("Found ", length(file_ids), " files in dataset folder.")

  # Validate files in parallel
  validation_results <- validate_entities_parallel(file_ids, mc.cores = mc.cores)

  # Collect validation errors
  results <- list()
  results$validation_error <- list()

  for (file_id in file_ids) {
    entity_val_result <- validation_results[[file_id]]
    if(!is.null(entity_val_result) &&
       !is.na(entity_val_result$isValid) &&
       entity_val_result$isValid == FALSE) {
      results$validation_error[[file_id]] <- entity_val_result
    }
  }

  results
}

#' Validate metadata of items in a collection
#'
#' This is usually used with a dataset collection.
#' Items in a collection inherit the schema binding from the collection,
#' so this function validates each item's annotations against the inherited schema
#' using Synapse's native validation services.
#'
#' @param collection_id Collection id.
#' @param mc.cores Number of cores to use for parallel validation.
#' Defaults to `min(parallel::detectCores() / 2, 4)` to be conservative.
#' Set to 1 to disable parallel processing.
#' @return A list with `validation_error` component containing any validation errors found.
#' @export
#' @aliases validate_dataset_items
validate_collection_items <- function(collection_id, mc.cores = NULL) {
  .check_login()
  coll <- .syn$get(collection_id)
  items <- coll$properties$datasetItems
  if(!length(items)) {
    warning("Collection has no items.")
    return(list(validation_error = list()))
  }
  item_ids <- sapply(items, `[[`, "entityId")
  message("Found ", length(item_ids), " items in collection.")

  # Validate items in parallel
  validation_results <- validate_entities_parallel(item_ids, mc.cores = mc.cores)

  # Collect validation errors
  results <- list()
  results$validation_error <- list()

  for (item_id in item_ids) {
    entity_val_result <- validation_results[[item_id]]
    if(!is.null(entity_val_result) &&
       !is.na(entity_val_result$isValid) &&
       entity_val_result$isValid == FALSE) {
      results$validation_error[[item_id]] <- entity_val_result
    }
  }

  results
}

#' @rdname validate_collection_items
#' @export
validate_dataset_items <- validate_collection_items


