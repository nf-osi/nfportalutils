#' Manifest-r
#'
#' (Manifest + faster) Given a dataset folder scope, quickly create a manifest or "reconstitute" a manifest when files have existing annotations.
#' The latter is similar to schematic's `manifest_generate` with option `useAnnotations = TRUE`.
#' This package *does* also facilitate using schematic's `manifest_generate`,
#' so here's what to keep in mind for where this might be preferred:
#'
#' - This implements a more convenient and faster method used for programmatic annotation workflows.
#' Schematic is meant to generate a human-facing doc for controlled, manual entry.
#' Thus, convenience and speed mainly comes from bypassing the schematic API layer and using the Synapse API directly,
#' and being able to reconstituting the manifest directly as R `data.table` instead of having to download create a Googlesheets or Excel intermediate.
#' The data is then augmented/filled out through other functions.
#' It is still easy to make a *schematic-compatible* manifest by using certain column naming rules.
#'
#' - Can choose between generating manifest with just immediate files *or* all nested files in scope.
#' This is useful in some situations.
#' For example, if the main scope folder is "RNA-seq" and the main files are fastqs, but there is one child folder called "md5" with md5 files,
#' and the manifest is really meant for the fastqs.
#' Schematic manifests only allow the latter.
#'
#' - Does not try to download "synapse_manifest.csv" like schematic does, which can avoid problems in some edge cases.
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
      manifest[, entityId := ids] # schematic-compatible
      manifest[, Filename := filenames] # schematic-compatible
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
