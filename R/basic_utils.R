#' Create local view of some scopes
#' 
#' Creating a fileview or dataset can be useful for filtering/selection,
#' but sometimes one wants to avoid having to do that directly on synapse,
#' because queries and query result downloads can be slower and not as facile.
#' As well, it can be somewhat messy to have many helper fileviews in a project.
#' This creates a local view with default props that works well enough for certain jobs.
#' 
#' @param scope Vector of one or more syn container ids.
#' @param idcol Name of an idcol; useful to keep track of which rows came from which scope. 
#' Can be NULL if not needed.
#' @param idmap Since id col will default to the syn id, 
#' providing a map will allow tracking by container name instead. 
#' @export 
local_view <- function(scope, idcol = "parent", idmap = NULL) {
  result <- list()
  for(i in scope) {
    children <- .syn$getChildren(parent = i)
    meta <- reticulate::iterate(children)
    result[[i]] <- data.table::rbindlist(meta)
  }
  if(!is.null(idmap)) names(result) <- idmap[names(result)]
  local_view <- data.table::rbindlist(result, idcol = idcol)
  return(local_view)
}

#' Create copy
#' 
#' Create a copy of syn entity; mostly used to create a copy on which to test out changes.
#' See https://python-docs.synapse.org/build/html/synapseutils.html?highlight=copy#synapseutils.copy_functions.copy
#' @param entity Entity to copy.
#' @param destination_id Id of destination project/container that entity will be copied to.
#' @param skip_copy_wiki_page Whether to skip copying wiki; defaults FALSE.
#' @param skip_copy_annotations Whether to skip copying annotations; defaults FALSE.
#' @export
copy <- function(entity, 
                 destination_id, 
                 skip_copy_wiki_page = FALSE, 
                 skip_copy_annotations = FALSE) {
  
  .check_login()
  # load synapseutils as needed -- so far this is the only util that uses it
  # TO DO: move to this to global package load?
  if(!reticulate::py_module_available("synapseutils")) stop("synapseutils module not available")
  synapseutils <- reticulate::import("synapseutils")
  
  synapseutils$copy(.syn, 
                    entity = entity, 
                    destinationId = destination_id, 
                    skipCopyWikiPage = skip_copy_wiki_page, 
                    skipCopyAnnotations = skip_copy_annotations)
  
}