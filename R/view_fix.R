#' Adapt view
#'
#' When a view schema and data are mismatched, the view cannot be built and therefore cannot be queried.
#' The most common causes (likely accounting for 98%+ of instances combined) will be the max size/length issues.
#' This will iteratively update the schema based exactly on whatever the server is saying to do* for the
#' sizing and list length issues, until the view is functional again and the query works.
#' However, if the issue is not one of these, this will fail because handlers for other, rarer problems are currently not implemented.
#'
#' *Note: We apply iterative fixes because that's just how server works currently in surfacing repair recommendations.
#'
#' @param view Synapse view id.
#' @param max_tries Number of tries. Most views should only have accumulated 1-2 mutations, so a default of 5 should be reasonable.
#' @export
adapt_view <- function(view, max_tries = 5L) {

  attempts <- 1

  while(attempts < max_tries) {

    res <- tryCatch(test <- .syn$tableQuery(glue::glue("SELECT * FROM {view} LIMIT 1")), error = function(e) return(e))
    need_fix <- "synapseclient.core.exceptions.SynapseHTTPError" %in% class(res)
    if(!need_fix) {
      message("view is in working order.")
      break
    }

    msg <- jsonlite::fromJSON(res$response$text)
    reason <- msg$reason
    if(!length(reason)) stop("The server did not return a usable reason.")

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
    col <- regmatches(hint, regexpr("(?<=\')(.*?)(?=\')", hint, perl = TRUE))
    size <-  regmatches(hint, regexpr("[0-9]+(?= characters)", hint, perl = TRUE))
    size <- tryCatch(as.integer(size), warning = function(w) stop("Unable to parse size from ", hint)) # convert warn to error if this is not a num
    message(glue::glue("Adapting size for '{col}'..."))

    # get schema and apply changes
    schema <- .syn$get(view)
    col_schema <- .syn$getTableColumns(schema) %>% reticulate::iterate()
    index <- match(col, sapply(col_schema, `[[`, "name"))
    if(is.na(index)) stop("Encountered issue finding relevant column in schema")
    ref_col <- col_schema[[index]]

    # Must create new immutable column with new size
    new_col <- col_schema[[index]]
    new_col <- synapseclient$Column(
      name = ref_col$name,
      columnType = ref_col$columnType,
      maximumSize = size)

    new_col_json <- jsonlite::toJSON(new_col, auto_unbox = TRUE)
    new_col <- .syn$restPOST(uri = "https://repo-prod.prod.sagebase.org/repo/v1/column", body = new_col_json)
    message(glue::glue("Created new column {new_col$id}"))

    # then update schema
    schema$removeColumn(ref_col$id)
    schema$addColumn(new_col$id)
    schema <- .syn$store(schema)
    message(glue::glue("Updated schema to use new column {new_col$id}"))

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

  # get schema and apply changes
  schema <- .syn$get(view)
  col_schema <- .syn$getTableColumns(schema) %>% reticulate::iterate()
  index <- match(col, sapply(col_schema, `[[`, "name"))
  if(is.na(index)) stop("Encountered issue finding relevant column in schema")
  ref_col <- col_schema[[index]]

  # new col def
  new_col <- col_schema[[index]]
  new_col <- synapseclient$Column(
    name = ref_col$name,
    columnType = ref_col$columnType,
    maximumListLength = len)

  new_col_json <- jsonlite::toJSON(new_col, auto_unbox = TRUE)
  new_col <- .syn$restPOST(uri = "https://repo-prod.prod.sagebase.org/repo/v1/column", body = new_col_json)
  message(glue::glue("Created new column {new_col$id}"))

  schema$removeColumn(ref_col$id)
  schema$addColumn(new_col$id)
  schema <- .syn$store(schema)
  message(glue::glue("Updated schema to use new column {new_col$id}"))

  return(schema$properties$id)

}
