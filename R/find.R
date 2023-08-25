# Utils to help overcome nested folders

#' Find in path
#' 
#' Get the Synapse id of an entity nested several folder layers deep without
#' having to click through the UI or create a fileview as long as the structure/path is known.
#' 
#' @param scope Id of the container (project or folder) to begin search.
#' @param path Path string in format "subdir1/subdir2/file.txt", where the last-level element will be the id returned.
find_in <- function(scope, path) {
  
  path_list <- strsplit(path, split = "/", fixed = TRUE)[[1]]
  here <- scope
  id <- NULL
  while(length(path_list)) {
    child <- first(path_list)
    path_list <- path_list[-1]
    here <- find_child(child_name = child, parent = here)
    id <- here
  }
  id
}

#' Find id of a child entity in a container
#'
#' @param parent Parent container (project or folder).
#' @param child_name Name of child entity.
find_child <- function(child_name, parent) {
  
  q <- .syn$getChildren(parent)
  child_id <- NULL
  repeat {
    x <- reticulate::iter_next(q) 
    if(is.null(x) || x$name == child_name) {
      child_id <- x$id
      break
    }
  }
  child_id
}

# Find nextflow assets --------------------------------------------------------- #

# Convenience functions for getting Synapse ids of nextflow assets

#' Find a standard nextflow workflow output asset
#' 
#' Note that samplesheets became part of the output only for newer versions of nf-core/rna-seq;
#' older runs may not find samplesheets. 
#' Paths default to known working paths corresponding to the latest major workflow version, 
#' but this may change and may need to be updated as part of util maintenance. 
#' 
#' @param syn_out Id of top-level folder that corresponds to `publishDir` in a nextflow workflow.
#' @param asset Name of asset to find.
#' @returns Id of samplesheet. 
#' @export
nf_find_asset <- function(syn_out, 
                          asset = c("software_versions", "multiqc_report", "samplesheet", "samtools_stats")) {
  
  asset <- match.arg(asset)
  path <- switch(asset,
                 software_versions = "pipeline_info/software_versions.yml",
                 multiqc_report = "multiqc/star_salmon/multiqc_report.html",
                 samplesheet = "pipeline_info/samplesheet.valid.csv",
                 samtools_stats = "multiqc/star_salmon/multiqc_data/multiqc_samtools_stats.txt"
  )
  
  id <- find_in(syn_out, path)
  if(is.null(id)) stop("File not found. Is this the right output directory/path?")
  id
}


#' Return workflow version according to workflow meta
#' 
#' @returns Version string. 
#' @export
nf_workflow_version <- function(syn_out) {
  
  version_meta <- nf_find_asset(syn_out, asset = "software_versions")
  file <- .syn$get(version_meta, downloadFile = TRUE)
  yml <- yaml::read_yaml(file$path)
  workflow <- grep("nf-core", names(yml$Workflow))
  yaml$Workflow[[workflow]]
  
}

