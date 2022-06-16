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

#' Create copy of entity
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


#' Copy annotation(s)
#' 
#' Copy annotations (all or selectively) from a source entity to one or more target entities.
#' If annotations already exist on target entities, the copy will replace the current values. 
#' 
#' @param entity_from Syn id from which to copy. 
#' @param entity_to One or more syn ids to copy annotations to. 
#' @param select Vector of properties to selectively copy if present on the entity. 
#' If not specified, will copy over everything, which may not be desired.
#' @param apply Whether to set copy on `entity_to` or return a table representation. 
#' @export
copy_annotation <- function(entity_from,
                            entity_to,
                            select = NULL,
                            apply = FALSE) {
  
  .check_login()
  
  annotations <- .syn$get_annotations(entity_from)
  if(is.null(select)) {
    copy <- annotations
  } else {
    copy <- reticulate::dict()
    for(k in names(annotations)) {
      if(k %in% select) copy[k] <- annotations[k]
    }
  }
  
  if(apply) {
    for(e in entity_to) {
      .syn$setAnnotations(e, annotations = copy)
    }
  } else {
    # reticulate::py_to_r(copy) does not work reliably, so convert to R obj manually
    x <- lapply(names(copy), function(x) list(copy[x]))
    names(x) <- names(copy)  
    x <- Map(rep, x, length(entity_to))
    dt <- data.table::as.data.table(x)
    dt[, entityId := entity_to]
    return(dt)
  }
}

