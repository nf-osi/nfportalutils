#' Re-manifest
#'
#' Quickly "reconstitute" a manifest for files with existing annotations.
#' This is a power-user alternative to schematic's `manifest_generate` with option `useAnnotations = TRUE` for certain scenarios:
#'
#' - Meant to be faster and more convenient for workflows focused on scalable/programmatic annotation/revalidation.
#' Convenience and speed mainly comes from bypassing the schematic API layer and using the Synapse API directly,
#' so manifest is reconstituted directly as R `data.table` **instead of having to download a Googlesheets or Excel intermediate**.
#' **The result `.csv` is still intended to be schematic-compatible for validation via `manifest_validate`**.
#'
#' - Works with both a Synapse dataset entity as well as a folder;
#' schematic currently only handles the latter (the folder concept predates Synapse datasets).
#'
#' - If folder-scoped, can choose between generating manifest with just immediate files *or*, like schematic, nested files.
#' This is useful, for example, if the main scope folder is "RNA-seq" and the main files are fastqs, but there is a child folder called "md5" with md5 files,
#' and the manifest is really meant for the fastqs.
#'
#' - Does not try to download `synapse_manifest.csv`, which can be useful to regenerate a manifest when ARs have been set.
#'
#' - Does not require an asset view.
#'
#' Other developer notes:
#' See also `gather_annotations` for the under-the-hood workhorse.
#' Intended for further development to be used as part of advanced annotation utils.
#'
#' @param scope Synapse id of something representing one or more datasets. Can be a folder *or* Synapse dataset entity.
#' @param file Name of file to be written. Use `NULL` to return the `data.table` instead of writing to file.
#' @param recurse Include everything nested within folder scope, default `TRUE`, or only the immediate files.
#' Not used if dataset is Synapse dataset entity.
#' @param mf Intermediate manifest to build upon; used when recursive.
#' @import data.table
#' @export
remanifest <- function(scope,
                       file = "manifest.csv",
                       recurse = TRUE,
                       mf = data.table()) {

  dataset <- .syn$get(scope[1])
  if(dataset$properties$concreteType == "org.sagebionetworks.repo.model.table.Dataset") {

    dataset_items <- sapply(dataset$properties$datasetItems, `[[`, "entityId")
    if(!length(dataset_items)) {

      return(data.table())

    } else {

      manifest <- gather_annotations(dataset_items)
      if(!is.null(file)) fwrite(manifest, file = file) else manifest

    }

  } else if(dataset$properties$concreteType == "org.sagebionetworks.repo.model.Folder") {

    files <- find_child_type(scope[1])
    if(length(files)) {
      ids <- files
      manifest <- gather_annotations(ids)
    } else {
      manifest <- data.table()
    }

    if(recurse) {
      more <- find_child_type(scope[1], list("folder"))
      scope <- c(scope[-1], more)
      if(!length(scope)) {
        manifest <- rbind(mf, manifest, fill = T)
        if(!is.null(file)) fwrite(manifest, file = file) else manifest
      } else {
        remanifest(scope = scope,
                   recurse = recurse,
                   mf = rbind(mf, manifest, fill = T))
      }
    } else {
      if(!is.null(file)) fwrite(manifest, file = file) else manifest
    }

  } else {

    "Dataset is only accepted as a folder or dataset entity. Please check the id used."

  }
}


#' Internal helper for gathering annotations into a table using the REST API
#'
#' This is an internal implementation that works directly with the platform service JSON
#' to afford more low-level control and avoid Python-R object conversion differences between reticulate versions.
#'
#' @param ids One or more ids.
#' @param list_sep List separator for list annotations.
#' @keywords internal
gather_annotations <- function(ids, list_sep = ", ") {
  results <- lapply(ids, function(id) .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{id}/annotations2")))
  annotations <- lapply(results, function(x) c(list(entityId = list(value = x$id)), x$annotations))
  annotations <- lapply(annotations, function(x) lapply(x, function(col) paste(col$value, collapse = list_sep)))
  manifest <- data.table::rbindlist(annotations, fill = TRUE)
  manifest
}


