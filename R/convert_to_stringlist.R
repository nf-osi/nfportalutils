#' Convert a delimited string to a stringlist annotation
#' 
#' Note that this schema change operation consults the current metadata model 
#' to help set schema parameter `max string length` 
#' (and possibly `max list length` in the future if that can be encoded in the model).  
#' This also serves as a built-in check that will throw an error 
#' if the data model doesn't recognize the key being changed.
#' However, to use this as a more general `max string length` util _without_ 
#' involving a metadata model, set `schema` to NULL and
#' `max string length` will be set based on the current values.
#' 
#' @description Converts a delimited string to a stringlist annotation and adjust the associated schema in the portal fileview.
#' @param fileview_id The synapse id of a fileview. 
#' Must have the desired annotations in the schema, 
#' and must have the files to annotate included in the scope. 
#' Must have write access to the files you want to re-annotate.
#' @param annotation_key A character string of the annotation you'd like to switch from a delimited string to a stringlist.
#' @param sep Default = ",". The delimiter in the character string.
#' @param trim_ws Default = TRUE. Remove white space at the beginning and end of list items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST result).
#' @param schema If not NULL, path to a readable .jsonld schema to use for setting new col schema. See details.  
#' @param dry_run Default = TRUE. Skips upload to table and instead prints study tibble.
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
                    columns = c("id", annotation_key), 
                    includeRowIdAndRowVersion = TRUE)
  
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
