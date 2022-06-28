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
#' @keywords internal
copy <- function(entity, 
                 destination_id, 
                 skip_copy_wiki_page = FALSE, 
                 skip_copy_annotations = FALSE) {
  
  .check_login()
  # load synapseutils as needed
  
  
  synapseutils$copy(.syn, 
                    entity = entity, 
                    destinationId = destination_id, 
                    skipCopyWikiPage = skip_copy_wiki_page, 
                    skipCopyAnnotations = skip_copy_annotations)
  
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

#' Download and read file to `data.table`
#'
#' Convenience function for reading a delimited local file or one on Synapse.
#' 
#' @keywords internal
#' @import data.table
dt_read <- function(file) {
  if(file.exists(file)) {
    path <- file
  } else if(grepl("^syn", file)) {
    message("Getting file from Synapse...")
    .check_login()
    path <- .syn$get(file)$path
  } else {
    stop("File must be local file or accessible synapse file.")
  }
  dt <- data.table::fread(path)
  return(dt)
}

#' Extract synapse id from URI or other string
#' 
#' @param uri URI or string containing embedded Synapse id.
#' @keywords internal
bare_syn_id <- function(uri) {
  not_na <- which(!is.na(uri))
  x <- uri[not_na]
  syn <- regmatches(x, regexpr("syn[0-9]{8}", x))
  uri[not_na] <- syn
  return(uri)
}


#' Walk through a directory
#' 
#' For now, an internal util imported from `synapseutils`.
#' @param syn_id Synapse id of directory root to traverse.
#' @param as_list 
#' @return An R list or Py generator object. 
#' @keywords internal
walk <- function(syn_id, as_list = TRUE) {
  .check_login()
  x <- synapseutils$walk(.syn, syn_id)
  if(as_list) reticulate::iterate(x) else x
  
}
