#' Set annotations from a manifest
#' 
#' The [Synapse docs](https://help.synapse.org/docs/Managing-Custom-Metadata-at-Scale.2004254976.html) 
#' suggest doing batch annotations through a fileview. However, it is often simpler to 
#' modify or set new annotations directly given a table of just the entities (rows) and props (cols) we want. 
#' This is similar to how schematic works, except without any validation 
#' (so it works best for power-users who know the data model very well).
#' Some desired defaults are taken into account,
#' such as not submitting key-values with `NA` and empty strings. 
#' 
#' @param manifest A table manifest. Needs to contain `entityId`.
#' @param ignore_na Whether to ignore annotations that are `NA`; default TRUE.
#' @param ignore_blank Whether to ignore annotations that are that empty strings; default TRUE.
#' @export   
annotate_with_manifest <- function(manifest, ignore_na = TRUE, ignore_blank = TRUE) {
  # Split by `entityId`
  annotations <- as.data.table(manifest)
  annotations[, Filename := NULL]
  annotations[, entityId := as.character(entityId)]
  annotations <- split(annotations, by = "entityId", keep.by = FALSE)
  filterNA <- if(ignore_na) function(x) !is.na(x) else TRUE
  filterBlank <- if(ignore_blank) function(x) x != "" else TRUE
  annotations <- lapply(annotations, function(x) Filter(function(x) filterNA(x) & filterBlank(x), as.list(x)))
  for(entity in names(annotations)) {
    .syn$setAnnotations(entity = entity, annotations = reticulate::r_to_py(annotations[[entity]]))
  }
}


#' Determine inherited properties
#' 
#' Internal function needed until there is better handling of property inheritance.
#' 
#' @inheritParams get_by_prop_from_json_schema
#' @param template URI of data template in model, prefixed if needed.
#' @keywords internal
inherit_props <- function(template, schema) {
  props <- get_dependency_from_json_schema(id = template, schema = schema)
  # Hard-coding props to NEVER inherit in the template
  select <- props[!props %in% c("comments", "entityId", "fileFormat", "dataType", "dataSubtype")]
  return(select)
}
