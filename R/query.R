#' Generic table query
#' 
#' @description  Retrieve selected data from a Synapse table.
#' @param table_id Synapse table id.
#' @param columns A character vector of selected columns (which often correspond to annotations, but not always).
#' If not given, will select all columns.
#' @param includeRowIdAndRowVersion Whether to include the row id and etag, defaults to `FALSE`. If the use case is to update rows in the table (rather than just to retrieve information for viewing, use `TRUE`).
#' @return A tibble.
#' @export
table_query <- function(table_id, columns = "*", includeRowIdAndRowVersion = F) {
  .check_login()
  
  table <- .syn$tableQuery(glue::glue('select {glue::glue_collapse(columns, sep = ",")} from {table_id}'),
                           includeRowIdAndRowVersion = includeRowIdAndRowVersion)$filepath %>%
    readr::read_csv(na=character())
  table
}

#' Consult schema about max string length 
#' 
#' Utility to query the schema regarding the max string length for a key
#' based on current valid values. If the key values are not constrained (free-text), 
#' a default string length of 100 is returned. If not in schema, returns `NA`.
#' TO DO: related fun to consult schema about type (integer, string, stringlist, etc.)
#' TO DO: warn if key is not actually string or stringlist type
#' @inheritParams get_valid_values_from_json_schema
#' @inheritParams key_label_to_id
#' @param key Schema key (not label).
#' @param default Default string length to use for keys without constrained values.
schema_max_str_len <- function(key,
                               schema = 'https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld',
                               parent_context = 'bts',
                               default = 100) {
  
  id <- key_label_to_id(key, prefixed = FALSE)
  if(!length(id)) return(NA_integer_)
  vals <- get_valid_values_from_json_schema(schema, parent_name = id)
  if(!length(vals)) return(default)
  chars <- sapply(vals, nchar)
  return(max(chars))
}

#' Query for schema key id given label
#' 
#' Utility to translate label to id using a schematic-generated schema.
#' 
#' @param label The term label, a.k.a display name.
#' @param prefixed Boolean to indicate whether to include namespace prefix or return bare ID. Defaults to `TRUE`.
#' @param schema URL or local path to a .jsonld file which the schema is to be read from. 
#' @return The id if found, such as "bts:MyID", otherwise an empty character vector.
#' @export
key_label_to_id <- function(label,
                            prefixed = TRUE,
                            schema = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld"
) {
  
  id <- jsonlite::fromJSON(schema) %>%
    purrr::pluck("@graph") %>%
    dplyr::filter(`sms:displayName` == label) %>%
    dplyr::pull(`@id`)
  
  if(prefixed) id else gsub("^.*:", "", id) # still returns empty character if not found
}