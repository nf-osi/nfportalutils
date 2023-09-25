#' Make manifest without using schematic API
#' 
#' This is meant for making or reconstituting a manifest (when using for Synapse files w/ existing annotations) given a scope. 
#' For the latter, it's similar to schematic's `manifest_generate` with option `useAnnotations = TRUE`. 
#' Why this exists and the main differences:
#' 
#' - Faster and more convenient way to generate a *schematic-compatible* manifest without having to use schematic.
#' "Schematic-compatible" means that if the current annotations were validated and submitted through schematic,
#' the reconstituted manifest should validate and submit successfully through schematic (given no bad changes made to the reconstituted version);
#' this can be tested by writing the table to csv and directly submitting through the API.
#' 
#' - Can choose between generating manifest with just immediate files *or* all nested files in scope. 
#' Currently, schematic forces the latter; this allows an extra option useful for certain situations.
#' 
#' - Does not look for stored "synapse_manifest.csv" like schematic does, which can avoid problems in some edge cases. 
#' 
#' - Manifest is reconstituted as R `data.table`, which can be turned into a csv, not Googlesheets or Excel interface. 
#' This is because this is often **used for programmatic annotation workflow**, not for manual annotation.
#'
#' @param scope Initial scope to use for generating file manifest, i.e. a dataset parent folder.
#' @param recurse Include everything nested within scope, default `TRUE`, or only the immediate files.
#' @param mf Manifest to build upon; used when recursive.
#' @param schematic_style Default to schematic-style column naming. See details.
#' @import data.table
#' @export
manifest_make <- function(scope, 
                          recurse = TRUE, 
                          mf = data.table(), 
                          schematic_style = TRUE) {
  
  files <- find_child_type(scope[1])
  if(length(files)) {
    ids <- files
    filenames <- names(files)
    annotations <- lapply(ids, function(i) .syn$get_annotations(i)) 
    manifest <- data.table::rbindlist(annotations, fill = TRUE)
    if(schematic_style) {
      manifest[, entityId := ids]
      manifest[, Filename := filenames]
    } else {
      manifest[, id := ids]
      manifest[, name := filenames]
    }
  } else {
    manifest <- data.table()
  }
  
  if(recurse) {
    more <- find_child_type(scope[1], list("folder"))
    scope <- c(scope[-1], more)
    if(!length(scope)) {
      rbind(mf, manifest, fill = T)
    } else {
      manifest_make(scope = scope, 
                    recurse = recurse, 
                    mf = rbind(mf, manifest, fill = T),
                    schematic_style = schematic_style)
    }
  } else {
    manifest
  }
}
