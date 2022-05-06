#' Convert a delimited string to a stringlist annotation
#' 
#' This is a schema change operation that updates 1) column type to list
#' and 2) sets a new `max string length` parameter of a Synapse Table 
#' (usually shrinking the max value). 
#' It can optionally consult the metadata model about a good `max string length`.
#' (and might handle `max list length` in the future if that could be encoded in the model as well).  
#' When a model is consulted, as a built-in check an error will be thrown 
#' if the data model doesn't recognize the key being changed,
#' i.e. when one wants to be strict about a key in the Table not documented by the model.
#' When there is no model to involve (`schema` = NULL), the `max string length` 
#' will simply be set based on the current values after processing the delimited list 
#' (the original code).
#' 
#' @param fileview_id The synapse id of a fileview. 
#' Must have the desired annotations in the schema, 
#' and must have the files to annotate included in the scope. 
#' Must have write access to the files you want to re-annotate.
#' @param annotation_key A character string of the annotation to switch from a delimited string to a stringlist.
#' @param sep The delimiter in the character string. Default = ",". 
#' @param trim_ws Remove white space at the beginning and end of list items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST result).
#' Default = TRUE. 
#' @param schema Optional, path to a readable .jsonld schema to use for setting new col schema. See details.  
#' @param dry_run Skip upload to table and instead prints study tibble. Default = TRUE.
#' @return If dry_run == T, returns list of updates and skips upload.
#' @export
convert_to_stringlist <- function(fileview_id, 
                                  annotation_key,
                                  sep = ",", 
                                  trim_ws = TRUE, 
                                  schema = NULL,
                                  dry_run = TRUE) {
  
  .check_login()
  
  fv <- table_query(fileview_id, 
                    columns = c("id", annotation_key))
  
  # Check whether there are stringlist values currently being represented as 
  # delimited strings
  fv_filt <- dplyr::filter(fv, .data[[annotation_key]] != "") %>%
    dplyr::filter(grepl(sep, .data[[annotation_key]]))
  updates <- list() 
  
  # Process values if there are any that need to be converted
  if(nrow(fv_filt)) {
    ls_annotations <- lapply(fv_filt[[annotation_key]], function(x) {
      .delim_string_to_vector(x, trim_ws = T, sep = sep)
    })
    
    names(ls_annotations) <- fv_filt$id
    updates <- ls_annotations
  }
  
  # Apply or only preview updates
  if(!dry_run) {
    # Set new annotations on entities
    if(length(updates)) { 
      lapply(names(updates), function(x) {
        .modify_annotation(synapse_id = x, key = annotation_key, value = updates[[x]])
      })
    }
    
    # Set new col schema based on whether a data model is involved
    if(!is.null(schema)) {
      max_str_len <- schema_max_str_len(annotation_key, schema = schema)
      if(is.na(max_str_len)) error("This key is not defined in schema.")
      # Also warn if current values exceed what data model says? 
    } else {
      max_str_len <- max(nchar(unlist(updates)))
    }
    .replace_string_column_with_stringlist_column(table_id = fileview_id,
                                                  column_name = annotation_key,
                                                  max_str_len = max_str_len)
    message(glue::glue("converted {annotation_key} in {fileview_id}"))
  
  } else {
    message(glue::glue("{length(updates)} files will be updated when dry_run = F."))
    return(updates)
  }
  
}

#' Convert a delimited string to vector, utility function.
#' @description Converts a delimited string to a stringlist annotation and adjust the associated schema in the portal fileview.
#' @param string A character string.
#' @param sep Default = ",". The delimiter in the character string.
#' @param trim_ws Default = TRUE. Remove white space at the beginning and end of list items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST result).
#' @export
.delim_string_to_vector <- function(string, sep, trim_ws = T){
  string <- as.character(string)
  if(trim_ws == T){
    strsplit(string, split = sep) %>%
      unlist %>%
      trimws
  }else{
    strsplit(string, split = sep) %>%
      unlist
  }
}

#' Modify a single annotation on a single file
#' @description Modifies a single annotation value on a single (existing) synapse file.
#' @param synapse_id A synapse entity id.
#' @param key The key of the annotation to modify.
#' @param value The value to change the annotation to.
#' @export
#'
.modify_annotation <- function(synapse_id, key, value){
  entity = .syn$get_annotations(synapse_id)
  entity[key] <- value
  .syn$set_annotations(entity)
}

#' Replace string column with stringlist column
#' @description Guts of this ripped from @jaeddy gist (https://gist.github.com/jaeddy/1cf49f7851945beedb39d431134734af)
#' @param table_id A synapse entity id.
#' @param column_name The column name of relevant column to modify.
#' @param max_str_len Max string length to be set in schema of new column.
#' @export
#'
.replace_string_column_with_stringlist_column <- function(table_id, column_name, max_str_len){

  col_id <- reticulate::iterate(.syn$getTableColumns(table_id))  %>%
    purrr::map(~ jsonlite::fromJSON(.$json())) %>%
    purrr::keep(~ .$name == column_name) %>%
    purrr::map_chr("id")

  old_column <- .syn$getColumn(col_id)

  if(old_column$columnType == "STRING"){
  new_column <- .syn$store(
    synapseclient$Column(
      name = rlang::as_string(column_name),
      columnType = "STRING_LIST",
      maximumSize = max_str_len,
      facetType = "enumeration"
    )
  )

  schema <- .syn$get(table_id)
  schema$removeColumn(old_column)
  schema$addColumn(new_column)

  schema <- .syn$store(schema)
  }
}
