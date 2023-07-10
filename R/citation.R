# -- Citations -----------------------------------------------------------------#

#' Get DOI metadata if it exists
#' 
#' Returns list of metadata associated with DOI if exists, otherwise NULL. 
#' Currently usable for certain entity types like files or datasets, 
#' though this should be revised to make more useful with other objects.
#' Note: Internal/experimental use only, not for production use.
#'
#' @param id Dataset data.
#' @keywords internal
get_doi_meta <- function(id) {
  
  # TODO Template query according to object type of id, 
  # i.e. folders can have dois, but they don't have version #s
  obj <- .syn$get(id, downloadFile = FALSE)
  versionNumber <- obj$properties$versionNumber # error if no versionNumber
  tryCatch({
    .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/doi?id={id}&type=DATASET&version={versionNumber}"))
  }, 
  error = function(e) if(grepl("DOI association does not exist.", e$message)) NULL else e)
}

#' Generate example dataset citation
#' 
#' This is currently more for demo purposes, to check how well current metadata 
#' could be formatted into citation text. Datasets where DOIs have been minted 
#' *or* NF-OSI processed datasets within the official Portal Collection should  
#' work well, while there are no guarantees for other cases.
#' Note: Internal/experimental use only, not for production use.
#'
#' @param id Dataset id.
#' @param format Currently just "Scientific Data" format.
#' @param output Currently only markdown, from which other utils can be used to generate LaTeX or HTML.
#' @keywords internal
cite_dataset <- function(id, 
                         format = "Scientific Data", 
                         output = c("markdown")) {
  if(!is_dataset(id)) stop("Not a dataset")
  if(length(get_doi_meta(id))) {
    message("For now, please go to https://citation.crosscite.org/ for the most comprehensive citation options.")
    return(NULL)
  } else {
    meta <- .syn$get_annotations(id)
    title <- meta$title
    creator <- meta$creator
    repository <- "Synapse"
    accession <- if(length(doi)) doi else glue::glue("https://www.synapse.org/#!Synapse:{id}") 
    yearPublished <- meta$yearPublished
    glue::glue("{creator}. _{repository}_ {accession} ({yearPublished}).")
  }
  
}

