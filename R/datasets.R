# -- Editing Collections -------------------------------------------------------#

# General helpers that should work for both datasets (collection of files) 
# and dataset collections (collection of datasets).


#' Structure as collection items
#' 
#' Helper taking entity ids to create records used for dataset items *or* dataset collection items.
#' Collection items have the form `list(entityId = id, versionNumber = x)`.
#'
#' Note: For item version, dataset items allow two meanings of literal or absolute "latest" 
#' vs. "stable_latest", but with files either one can be used to mean the same thing
#' since there will be correct interpretation done under the hood.
#' See implementation in `latest_version`.
#'
#' @param ids Ids of entities to make into dataset items.
#' @param item_version Integer for version that will be used for all items, e.g. 1. 
#' Otherwise, "latest" or "stable_latest". See details.
#' @keywords internal
as_coll_items <- function(ids, item_version = c("abs", "stable")) {

  if(!is.integer(item_version)) {
    version_semantics <- match.arg(item_version)
    item_version <- lapply(ids, function(id) latest_version(id, version_semantics))
  }

  items <- Map(function(id, version) list(entityId = id, versionNumber = version), ids, item_version)
  names(items) <- NULL # need to unname list for API
  items
}


#' Apply updates to current collection of items
#' 
#' This is essentially an internal transaction helper for trying to apply a changeset to a collection,
#' used in several higher-level collection utils. 
#' Given the changeset that can represent updates of both types "replace" or "add",
#' this applies an update join keyed on `entityId` for the replace and 
#' appends the new items to get the updated collection.
#' 
#' @param current_items List of lists representing a collection of items.
#' @param update_items Collection of items to apply as updates to `current_items`. 
#' @keywords internal
update_items <- function(current_coll, update_coll) {
  
  current_coll <- data.table::rbindlist(current_coll)
  update_coll <- data.table::rbindlist(update_coll)
  replaced <- current_coll[update_coll, on = .(entityId), versionNumber := i.versionNumber]
  added <- update_coll[!current_coll, on = .(entityId)]
  updated <- rbind(replaced, added)
  # reconversion; using pure apply as.list coerces versionNumber into char
  updated <- apply(updated, 1, function(i) list(entityId = unname(i[1]), versionNumber = as.integer(i[2]))) 
  updated
}


#' Update item versions to "latest" in a collection
#' 
#' Update an _existing_ collection so that all items or a subset of items reference their latest version.
#' Should work for both datasets (collection of files) and dataset collections (collection of datasets).
#' 
#' @inheritParams latest_version
#' @param collection_id Collection id.
#' @param items Vector of dataset ids for which to update reference to latest version, or "all" (default) to update all.
#' @export
use_latest_in_collection <- function(collection_id, items = "all", version_semantics = "abs") {

  coll <- .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{collection_id}"))
  current_items <- sapply(coll$items, function(i) i$entityId)

  if((length(items) == 1) && (items  == "all")) {
    coll$items <- as_coll_items(current_items, item_version = version_semantics)
  } else {
    
    # Check subset; if no check, this becomes `add_to_collection`
    if(!all(items %in% current_items)) {
      warning("Subset given includes items not actually in collection: ", items[!items %in% current_items])
      items <- items[items %in% current_items]
      if(!length(items)) {
        warning("No qualifying items to update. No updates applied.")
        return(coll)
      }
    }
    updated_items <- update_items(coll$items, as_coll_items(items, item_version = version_semantics))
    coll$items <- updated_items
  }
  .syn$restPUT(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{collection_id}"), body = jsonlite::toJSON(coll, auto_unbox = TRUE))
  
}


#' Add to collection
#'
#' Add items(s) to an _existing_ collection, using the item(s)' current (latest) version.
#' For datasets, the items should be files. For dataset collections, the items should be datasets.
#' If an item attempting to be added happens to already be in the collection,
#' this might lead to version conflicts, so the update will be rejected unless `force` is true.
#' 
#' This is implemented with lower-level REST API because the Python client (as of v2.7) doesn't yet 
#' implement dataset collection class and methods (but dataset and relevant methods like `add_item` method are available).
#' Thus, while this is generic enough to handle both datasets and dataset collections 
#' it is expected to be used more for dataset collections given that the dataset method is provided.
#' 
#' @param collection_id Collection id.
#' @param items Character vector of one or more dataset entity ids to add.
#' @param check_items Whether to check that ids are really appropriate item types and remove non-appropriate item types
#' to help avoid Synapse errors (default `FALSE` because in most cases `items` are curated, and using check will be slower).
#' @param force If some items are currently in the collection with a different version, 
#' should these items be force-added using current version? The safe default is `FALSE` to ensure any such updates are intentional.
#' @export
add_to_collection <- function(collection_id, items, check_items = FALSE, force = FALSE) {
  
  coll <- .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{collection_id}"))
  coll_type <- which_coll_type(coll)
  
  if(check_items) {
    item_type_check <- if(coll_type == "dataset") is_file else is_dataset
    correct_item_type <- sapply(items, item_type_check)
    if(any(!correct_item_type)) {
      warning("Some items not correct entity types for the collection and will not be added: ", items[!correct_item_type])
      items <- items[correct_item_type]
      if(!length(items)) {
        warning("No qualifying items to add. No updates applied.", call. = FALSE)
        return(coll)
      }
    }
  }
  
  current_items <- sapply(coll$items, function(x) x$entityId)
  if(any(items %in% current_items) && !force) {
    stop("Some items to be added are already in collection. Use `force = TRUE` to allow replacing existing versions.")
  } else {
    coll$items <- update_items(coll$items, as_coll_items(items))
  }
  .syn$restPUT(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{collection_id}"), body = jsonlite::toJSON(coll, auto_unbox = TRUE))
}


# -- Datasets ------------------------------------------------------------------#

#' Create new dataset with given items
#'
#' @inheritParams as_coll_items
#' @param name Name of the dataset. It should be unique within the `parent` project.
#' @param parent Synapse id of parent project where the dataset will live.
#' @param items Id(s) of items to include.
#' Usually the same parent project storing the files, but in some cases it may be a different project.
#' @param dry_run If TRUE, don't actually store dataset, just return the data object for inspection or further modification.
#' @export
new_dataset <- function(name, parent, items, item_version = NULL, dry_run = TRUE) {

  dataset_items <- as_coll_items(items, item_version)
  dataset <- synapseclient$Dataset(name = name,
                                   parent = parent,
                                   dataset_items = dataset_items)
  if(dry_run) dataset else .syn$store(dataset)
}


#' Get the latest version
#'
#' Get latest version, with special handling for semantics of "latest" regarding new collection types.
#' Datasets and dataset collections always start out as draft so unlike other entities
#' there is a concept of a stable version which is the "real" latest, but which might not always exist.
#' For datasets/dataset collections the latest version refers to a DRAFT, so latest stable version is `versionNumber` - 1
#' under the condition that the `versionNumber` is greater or equal to 2.
#' When `versionNumber` = 1 and `isLatestVersion` is TRUE, this means there is not yet a stable version.
#' When using stable version semantics, if a stable version does not exist an error will be thrown.
#'
#' The parameter `version_semantics` allows user to specify "what type of *latest* do you mean?".
#'
#' Note: Do not use with versioned ids of the form "syn12345678.3"
#'
#' @param id Dataset id. See details.
#' @param version_semantics Use "abs" for absolute latest version or "stable". Only used for collection entities. See details.
latest_version <- function(id, version_semantics = c("abs", "stable")) {

  entity <- .syn$get(id, downloadFile = FALSE)
  version <- entity$properties$versionNumber
  if(entity$properties$concreteType %in% c("org.sagebionetworks.repo.model.table.Dataset", "org.sagebionetworks.repo.model.table.DatasetCollection")
     && version_semantics == "stable_latest") {
     version <- version - 1
     if(!version) stop("No stable version exists for ", id)
  }

  version
}


#' Create datasets for Sarek-called somatic or germline variants results
#' 
#' Organize variant call files from Nextflow Sarek into 3-4 datasets, 
#' grouping files by variant type and workflow with titles having the format: 
#' "{type} Genomic Variants - {workflow} Pipeline", e.g. "Somatic Genomic Variants - Strelka Pipeline".
#' As you can see, this assumes that you want to create datasets that segregate Somatic and Germline calls. 
#' This makes sense for NF because Germline calls can be treated differently.  
#' This uses latest version of all files and creates a Draft version of the dataset.
#' 
#' Since we basically just need the syn entity id, variant type, and workflow to group the files. 
#' Instead of getting this info through running `map_*` as in the example,
#' you may prefer using a fileview, in which case you just need to download a table from a fileview 
#' that has `id` => `output_id` + the `dataType` and `workflow` annotations. 
#' The fileview can be used _after_ the files are annotated. If you want to create datasets _before_
#' files are annotated, then you have to use `map_*`.
#' 
#' Finally, datasets cannot use the same name if stored in the same project,
#' so if there are multiple batches, the names will have to be made unique by adding
#' the batch number, source data id, processing date, or whatever makes sense.
#' 
#' @inheritParams new_dataset
#' @param output_map The `data.table` returned from `map_sample_output_sarek`. See details for alternatives.
#' @param workflow One of workflows used.
#' @param verbose Optional, whether to be verbose -- defaults to TRUE.
#' @import data.table
#' @return A list of dataset objects.
#' @export
#' @examples
#'\dontrun{
#' syn_out <- "syn26648589"
#' m <- map_sample_output_sarek(syn_out)
#' datasets <- nf_sarek_datasets(m, parent = "syn26462036", dry_run = F) # use a test project
#'}
nf_sarek_datasets <- function(output_map, 
                              parent, 
                              workflow = c("FreeBayes", "Mutect2", "Strelka", "DeepVariant"),
                              verbose = TRUE, 
                              dry_run = TRUE) { 
  
  output_map <- as.data.table(output_map)
  if(!is.null(output_map$dataType)) {
    data_type <- unique(output_map$dataType)
    if(length(data_type) != 1) stop("Expecting one `dataType`, which does not appear to be the case.")
    gvtype <- grep("(Germline|Somatic)Variants", data_type, value = T)
    if(!length(gvtype)) stop("Data type does not look right, expecting either Germline or Somatic variants.")
    gvtype <- switch(gvtype,
                     SomaticVariants = "Somatic",
                     GermlineVariants = "Germline")
    
  } else {
    # Detect genomic variants type from first path name 
    gvtype <- if(grepl("SomaticVariantCalls", first(output_map$caller_path))) {
      "Somatic"  
    } else if(grepl("GermlineVariantCalls", first(output_map$caller_path)))  {
      "Germline"
    } else {
      stop("Could not assign either Germline or Somatic labels based on main output folder. 
           Check whether folder contains mixed types or is not the right one.")
    }
  }
  pattern <- "vcf.gz(.tbi)?$"
  workflow <- match.arg(workflow)
  datasets <- list()
  for(i in workflow) {
    dataset <- output_map[workflow == i & grepl(pattern, output_name)]
    if(nrow(dataset)) {
      if(verbose) glue::glue("Creating {i} dataset with {nrow(dataset)} files")
      name <- glue::glue("{gvtype} Genomic Variants - {i} Pipeline")
      dataset <- new_dataset(name = name, parent = parent, items = dataset$output_id, dry_run = TRUE)
      if(dry_run) datasets[[i]] <- syn_dataset else datasets[[i]] <- .syn$store(syn_dataset)
    }
  }
  
  return(datasets)
  
}


#' Create dataset for STAR-Salmon expression quantification results
#' 
#' With a level-3 manifest that is created from `annotate_expression`,
#' calls `new_dataset` to make quantification files (.sf) into dataset. 
#' Uses latest version of the files and creates a "Draft" dataset.
#' See `nf_sarek_datasets`.
#' 
#' @inheritParams new_dataset
#' @inheritParams nf_sarek_datasets
#' @param manifest A table of annotated data manifest from `annotate_expression`.
#' @export
nf_star_salmon_datasets <- function(manifest, 
                                    parent,
                                    dry_run = TRUE) { 
  
  items <- manifest$entityId
  new_dataset(name = "Gene Expression Quantification from RNA-seq",
              parent = parent,
              items = items,
              dry_run = dry_run)
}

#' Create dataset for CNVKit results
#'
#' Create dataset from all files in CNVKit output
#'
#' @inheritParams new_dataset
#' @param syn_out Output folder called 'cnvkit'
#' @export
nf_cnv_dataset <- function(syn_out,
                           parent,
                           dry_run = TRUE) {

  files <- walk(syn_out)
  files <- unlist(files)
  df <- as.data.frame(matrix(files, ncol = 2, byrow = TRUE))
  names(df) <- c("Filename", "id")
  df <- df[grepl("cnr$|cns$|cnn$|bed$|pdf$|png$", df$Filename), ]
  items <- df$id
  new_dataset(name = "Copy Number Variant - CNVkit",
              parent = parent,
              items = items,
              dry_run = dry_run)
}


# -- Checks------------- -------------------------------------------------------#

# TODO Potentially move these type checks somewhere else like basic_utils
# TODO Better composition to reduce code, esp. if more will be added

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

