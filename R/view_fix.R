#' Adapt view
#'
#' When a view schema and data are mismatched, the view cannot be built and therefore cannot be queried.
#' The most common causes (likely accounting for 98%+ of instances combined) will be the max size/length issues.
#' This will iteratively update the schema based exactly on whatever the server is saying to do* for the
#' sizing and list length issues, until the view is functional again and the query works.
#' However, if the issue is not one of these, this will fail because handlers for other, rarer problems are currently not implemented.
#'
#' *Note: Fixes are applied iteratively because that's how server currently surfaces repair recommendations.
#'
#' @param view Synapse view id.
#' @param max_tries Number of tries. Vast majority of views should only have accumulated 1-2 bad data mutations, so a default of 5 should be reasonable.
#' @export
adapt_view <- function(view, max_tries = 5L) {

  attempts <- 1

  while(attempts < max_tries) {

    res <- tryCatch(test <- .syn$tableQuery(glue::glue("SELECT * FROM {view} LIMIT 1")), error = function(e) return(e))
    need_fix <- "synapseclient.core.exceptions.SynapseHTTPError" %in% class(res)
    if(!need_fix) {
      message("View is in working order.")
      break
    }

    msg <- jsonlite::fromJSON(res$response$text)
    reason <- msg$reason
    if(!length(reason)) stop(glue::glue("Server returned unknown response: {msg}"))

    if(grepl("too small", reason)) {
      view <- adapt_size(view, hint = reason)
      attempts <- attempts + 1
    }

    if(grepl("maximumListLength", reason)) {
      view <- adapt_list_length(view, hint = reason)
      attempts <- attempts + 1
    }
  }
}

#' Adapt schema max size based on hint
#'
#' @keywords internal
adapt_size <- function(view, hint) {

    # parse to extract column and resizing recommendation
    col_name <- regmatches(hint, regexpr("(?<=\')(.*?)(?=\')", hint, perl = TRUE))
    size <-  regmatches(hint, regexpr("[0-9]+(?= characters)", hint, perl = TRUE))
    # convert warn to error if this is not a num
    size <- tryCatch(as.integer(size), warning = function(w) stop("Unable to parse size from ", hint))
    message(glue::glue("Adapting size for '{col_name}'..."))

    # get schema and apply changes; must create new immutable column with diff size
    schema <- .syn$get(view)
    new_col <- match_col(schema, col_name)
    new_col$id <- NULL
    new_col$maximumSize <- size
    new_col <- new_col(new_col)

    # then update schema
    schema <- swap_col(schema, old = ref_col$id, new = new_col$id)

    return(schema$properties$id)
}


#' Adapt schema max list length based on hint
#'
#'
#' @keywords internal
adapt_list_length <- function(view, hint) {

  # parse to extract column length recommendation
  col <- regmatches(hint, regexpr("(?<=\")(.*?)(?=\")", hint, perl = TRUE))
  len <-  regmatches(hint, regexpr("[0-9]+$", hint, perl = TRUE))
  len <- tryCatch(as.integer(len), warning = function(w) stop("Unable to parse length from ", hint))
  message(glue::glue("Adapting list length for '{col}'..."))

  # get schema and apply changes; new col def with diff in maximumListLength
  schema <- .syn$get(view)
  new_col <- match_col(schema, col_name)
  new_col$id <- NULL
  new_col$maximumListLength <- len
  new_col <- new_col(new_col)

  schema <- swap_col(schema, old = ref_col$id, new = new_col$id)

  return(schema$properties$id)

}

#- Helpers ---------------------------------------------------------------------#

#' Find matching col in schema based on name
#'
#' Synapse doesn't allow schemas to have columns of same name,
#' so this should never return more than one.
#' @keywords internal
match_col <- function(schema, col_name) {

  col_schema <- .syn$getTableColumns(schema) %>% reticulate::iterate()
  index <- match(col_name, sapply(col_schema, `[[`, "name"))
  if(is.na(index)) stop("Encountered issue finding relevant column in schema")
  ref_col <- col_schema[[index]]
  ref_col

}

#' Create new col
#'
#' @param col Column definition represented as a list.
#' @keywords internal
new_col <- function(col) {

  new_col_json <- jsonlite::toJSON(col, auto_unbox = TRUE)
  new_col <- .syn$restPOST(uri = "https://repo-prod.prod.sagebase.org/repo/v1/column", body = new_col_json)
  message(glue::glue("Created new column {new_col$id}"))
  new_col

}

#' Swap out old column for a new column in a schema
#'
#' @param schema A table schema.
#' @param old Id of old col.
#' @param new Id of new col
#' @keywords internal
swap_col <- function(schema, old, new) {

  schema$removeColumn(old)
  schema$addColumn(new)
  schema <- .syn$store(schema)
  message(glue::glue("Updated schema to use new column {new}"))
  schema
}
