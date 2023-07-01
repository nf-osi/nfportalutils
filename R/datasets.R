#' As dataset items
#' 
#' Helper taking entity ids to create records in the structure needed for dataset items or dataset collection items.
#'
#' @param ids Ids of entities to make into dataset items.
#' @param item_version Integer for version that will be used for all items, e.g. 1. 
#' If NULL, this will look up the latest version for each id and use that.
as_dataset_items <- function(ids, item_version = NULL) {
  if(is.null(item_version)) {
    item_version <- lapply(ids, function(id) .syn$get(id, downloadFile = FALSE)$properties$versionNumber)
  }
  dataset_items <- Map(function(id, version) list(entityId = id, versionNumber = 1L), ids, item_version)
  names(dataset_items) <- NULL # need to unname list for API
  dataset_items
}

#' New dataset with given items
#'
#' Make a _new_ dataset with given set of entities.
#'
#' @inheritParams as_dataset_items
#' @param name Name of the dataset. It should be unique within the `parent` project.
#' @param parent Synapse id of parent project where the datasets will live.
#' @param items Id(s) of items to include.
#' Usually the same parent project storing the files, but in some cases it may be a different project.
#' @param dry_run If TRUE, don't actually store dataset, just return the data object for inspection or further modification.
new_dataset <- function(name, parent, items, item_version = NULL, dry_run = TRUE) {

  dataset_items <- as_dataset_items(items, item_version)
  dataset <- synapseclient$Dataset(name = name,
                                   parent = parent,
                                   dataset_items = dataset_items)
  if(dry_run) dataset else .syn$store(dataset)
}

#' Create Sarek-processed datasets
#' 
#' Organize variant call files from Nextflow Sarek into 3-4 datasets, 
#' grouping files by variant type and workflow with titles having the format: 
#' "{type} Genomic Variants - {workflow} Pipeline", e.g. "Somatic Genomic Variants - Strelka Pipeline".
#' As you can see, this assumes that you want to create datasets that segregate Somatic and Germline calls. 
#' This makes sense for NF because Germline calls can be treated differently.  
#' This uses version 1 of all files and creates a Draft version of the dataset.
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


#' Create NF STAR-Salmon dataset
#' 
#' Organize gene expression quantification files (.sf) into one dataset. 
#' Uses version 1 of the files and creates a "Draft" dataset.
#' See also `nf_sarek_datasets`.
#' 
#' @inheritParams new_dataset
#' @inheritParams nf_sarek_datasets
#' @param output_map The `data.table` returned from `map_sample_output_sarek`.
#' @export
#' @examples
#'\dontrun{
#' syn_out <- "syn30840584"
#' m <- map_sample_output_rnaseq(syn_out) 
#' datasets <- nf_rnaseq_dataset(m, out, parent = "syn4939902", dry_run = F)
#'}
nf_star_salmon_datasets <- function(output_map, 
                                    parent, 
                                    verbose = TRUE, 
                                    dry_run = TRUE) { 
  
  # Select the .sf and index files
  output_ids <- output_map[grepl(".sf$", output_name), output_id]
  new_dataset(name = "Gene Expression Quantification from RNA-seq",
              parent = parent,
              items = output_ids,
              dry_run = dry_run)
}

# -- Checks------------- -------------------------------------------------------#

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


# -- Dataset Collections -------------------------------------------------------#

#' Add to dataset collection
#' 
#' Add dataset(s) to an _existing_ dataset collection.
#' Implemented with lower-level REST API because the Python client (as of v2.7) doesn't yet 
#' implement an `add_scope`-type method for dataset collections that is available for entity view.
#' 
#' @param items Character vector of one or more dataset entity ids to add, using their current version. 
#' @param collection_id Id of the dataset collection.
#' @param check_items Whether to check that ids are really dataset entities and remove non-dataset entities with warning (default FALSE). 
#' This may be useful given that sometimes "datasets" can be folder or file entities. Note that using check will be slower.
#' @param replace If specified items are current items in the collection, replace items with the current version?
#' The safe default is FALSE to ensure any version changes are intentional.
add_to_dataset_collection <- function(items, collection_id, check_items = FALSE, replace = FALSE) {
  
  if(check_items) {
    confirmed_dataset <- sapply(items, is_dataset)
    if(any(!confirmed_dataset)) {
      warning("Items which are not dataset entities will be ignored:", items[!confirmed_dataset])
      items <- items[confirmed_dataset]
    }
  }
  dc <- .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{collection_id}"))
  current_items <- sapply(dc$items, function(i) i$entityId)
  
  # Synapse will normally throw error if trying to add a dataset already in collection
  if(any(items %in% current_items) && !replace) {
    stop("Datasets to be added are already in collection. Use `replace = TRUE` if you want to override existing dataset versions.") 
  } else if (any(items %in% current_items && replace)) {
    dc$items <- as_dataset_items(union(current_items, items))
    message("Some datasets replaced with their most current version:", items[items %in% current_items])
  } else {
    dc$items <- c(dc$items, as_dataset_items(items))
  }
  .syn$restPUT(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{collection_id}"), body = jsonlite::toJSON(dc, auto_unbox = TRUE))
}

# ------------------------------------------------------------------------------#
