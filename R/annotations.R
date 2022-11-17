#' Set annotations from a manifest
#' 
#' The [Synapse docs](https://help.synapse.org/docs/Managing-Custom-Metadata-at-Scale.2004254976.html) 
#' suggest doing batch annotations through a fileview. However, it is often simpler to 
#' modify or set new annotations directly given a table of just the entities (rows) and props (cols) we want. 
#' This is like how schematic works, except without any validation (so works best for power-users who know the data model well).
#' Some desired defaults are taken into account, such as not submitting key-values with `NA` and empty strings. 
#' 
#' @param manifest A table manifest. Needs to contain `entityId`.
#' @param ignore_na Whether to ignore annotations that are `NA`; default TRUE.
#' @param ignore_blank Whether to ignore annotations that are that empty strings; default TRUE.
#' @export   
annotate_with_manifest <- function(manifest, ignore_na = TRUE, ignore_blank = TRUE) {
  # Split by `entityId`
  annotations <- as.data.table(manifest)
  if("Filename" %in% names(annotations)) annotations[, Filename := NULL]
  annotations[, entityId := as.character(entityId)]
  annotations <- split(annotations, by = "entityId", keep.by = FALSE)
  filterNA <- if(ignore_na) function(x) !any(is.na(x)) else TRUE # will ignore entirely if list with NA, e.g. c(NA, 1, 2) -- should warn if list
  filterBlank <- if(ignore_blank) function(x) !any(x == "") else TRUE # same as above
  annotations <- lapply(annotations, function(x) Filter(function(x) filterNA(x) & filterBlank(x) & length(x), unlist(x, recursive = F)))
  for(entity in names(annotations)) {
    .syn$setAnnotations(entity = entity, annotations = as.list(annotations[[entity]]))
  }
}


#' Copy annotations
#' 
#' Copy annotations (all or selectively) from a source entity to one or more target entities.
#' If annotations already exist on target entities, the copy will replace the current values. 
#' 
#' @param entity_from Syn id from which to copy. 
#' @param entity_to One or more syn ids to copy annotations to. 
#' @param select Vector of properties to selectively copy if present on the entity. 
#' If not specified, will copy over everything, which may not be desirable.
#' @param update Whether to immediately update or return annotation objects only. 
#' @export
copy_annotations <- function(entity_from,
                             entity_to,
                             select = NULL,
                             update = FALSE) {
  
  .check_login()
  
  annotations <- .syn$get_annotations(entity_from)
  if(is.null(select)) {
    cp <- annotations
  } else {
    cp <- reticulate::dict()
    for(k in names(annotations)) {
      if(k %in% select) cp[k] <- annotations[k]
    }
  }
  
  if(update) {
    for(e in entity_to) {
      .syn$setAnnotations(e, annotations = cp)
    }
  } else {
    return(cp)
  }
}


#' QC a derived manifest
#' 
#' Check missing annotations, usually because input files are missing values so nothing could be copied.
#' It might also be helpful to visualize the manifest via a package like `naniar::vis_miss(manifest)`.
#' 
#' @param manifest A manifest, usually from one of the `annotate_*` functions.
#' @param sample_io Input/output mapping used; only used if missing annotations detected.
#' @return NULL if no problems, otherwise a table of entity ids affected, the attributes missing, and inputs used. 
#' @export
qc_manifest <- function(manifest, sample_io) {
  missing <- apply(manifest, 2, function(x) sum(is.na(x)))
  if(all(missing == 0)) {
    message("All values are present in manifest")
    return()
  } else {
    message("Some values not present in manifest...")
    na_attrs <- apply(manifest, 1, function(x) names(x[is.na(x)]))
    na_subset <- manifest[lengths(na_attrs) > 0, .(entityId)]
    na_subset[, attributes := sapply(na_attrs[lengths(na_attrs) > 0], paste, collapse = ",")]
    na_subset <- merge(na_subset, sample_io[, .(output_id, output_name, input_id, sample)], by.x = "entityId", by.y = "output_id")
    return(na_subset)
  }
}


