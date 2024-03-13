#' Copy a table
#'
#' Copy a table. One of the most common use cases is testing, to avoid modifying a "production" table.
#'
#' @param table_id Id of table to copy.
#' @param destination_id Parent project id for the copy.
#' @export
copy_table <- function(table_id,
                       destination_id) {

  message(glue::glue("Getting table {table_id}"))
  schema <- synapser::synGet(table_id)
  data <- synapser::synTableQuery(glue::glue("select * from {table_id}"), includeRowIdAndRowVersion = FALSE)
  columns <- schema$columnIds
  schema_copy <- synapser::Schema(name = schema$name, parent = destination_id, columns = columns)
  table_copy <- synapser::Table(schema = schema_copy, values = data$filepath)
  table_copy <- synapser::synStore(table_copy)
  message(glue::glue("Copied table {table_id} to {table_copy$tableId}"))
  table_copy$tableId
}


#' Download and read file to `data.table`
#'
#' Convenience function for reading a delimited local file or one on Synapse.
#'
#' @param file File Synapse id or local path.
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
  syn <- regmatches(x, regexpr("syn[0-9]{8,12}", x))
  uri[not_na] <- syn
  return(uri)
}

#' Validate a Synapse ID
#'
#' Returns the id if valid, throws error if not.
#' @param id Id string.
#' @keywords internal
is_valid_syn_id <- function(id) {
  result <- grepl("^syn[0-9]{8,12}$", id)
  result
}

#' Walk through a directory
#'
#' For now, an internal util imported from `synapseutils`.
#' @param syn_id Synapse id of directory root to traverse.
#' @param as_list Whether to return as R list.
#' @return An R list or Py generator object.
#' @keywords internal
walk <- function(syn_id, as_list = TRUE) {
  .check_login()
  x <- synapseutils$walk(.syn, syn_id)
  if(as_list) reticulate::iterate(x) else x

}


#' Adds a row to a table.
#' @param schema A synapse table Schema object.
#' @param new_row A data frame of one or more rows that match the provided schema.
#' @export
.store_rows <- function(schema, new_row){

  table <- synapser::synStore(synapser::Table(schema, new_row))

}

# .pluck_column_type_and_name <- function(column){
#   coltype <- purrr::pluck(column, "columnType")
#   name <- purrr::pluck(column, "name")
#
#   c(coltype, name)
# }

