#' Generic table query
#' 
#' @description  Retrieve selected data from a Synapse table.
#' @param table_id Synapse table id.
#' @param columns A character vector of selected columns (which often correspond to annotations, but not always).
#' If not given, will select all columns.
#' @param includeRowIdAndRowVersion Whether to include the row id and etag, defaults to `FALSE`. If the use case is to update rows in the table (rather than just to retrieve information for viewing, use `TRUE`).
#' @example studies <- table_query("syn16787123")
#' @result A tibble.
#' @export
table_query <- function(table_id, columns = "*", includeRowIdAndRowVersion = F) {
  .check_login()
  
  table <- .syn$tableQuery(glue::glue('select {glue::glue_collapse(columns, sep = ",")} from {table_id}'),
                           includeRowIdAndRowVersion = F)$filepath %>%
    readr::read_csv(na=character())
  table
}