#' Add to scope
#'
#' Convenience function to add container to view scope.
#'
#' @param view_id Id of view
#' @param container_id Id of container to add.
#' @export
add_to_scope <- function(view_id, container_id) {

  view <- .syn$get(view_id)
  new_scope_id <- sub("syn", "", container_id) # should be integer id
  view$add_scope(new_scope_id)
  view <- .syn$store(view)
  invisible(view)
}


#' Convert delimited record to JSON representation needed by a stringlist col schema
#'
#' Internal helper that reuses and extends the utility of `.delim_string_to_vector`.
#'
#' @inheritParams .delim_string_to_vector
#' @param record Character vector of length one representing a single record.
#' @keywords internal
strlist_JSON <- function(record, sep = ",", trim_ws = T) {
  .delim_string_to_vector(string = record, sep, trim_ws = T) %>%
    jsonlite::toJSON() %>%
    as.character()
}
