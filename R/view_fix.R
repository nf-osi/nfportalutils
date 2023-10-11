#' Adjust view
#'
#' When a view schema and data are mismatched, the view cannot be built and therefore cannot be queried.
#' The most common causes (likely accounting for 98%+ of instances combined) will be the max size/length issues.
#' This will iteratively update the schema based exactly on whatever the server is saying to do* for the
#' sizing and list length issues, until the view is functional and querying works.
#' However, if the issue is not one of these, this will fail because handlers for other problems are currently not implemented.
#'
#' *Note: Fixes are applied iteratively because that's how server currently surfaces repair recommendations.
#'
#' @param view Synapse view id.
#' @param max_tries Max number of tries. Vast majority of views should only have accumulated 1-2 bad data mutations, so a default of 5 should be reasonable.
#' @param check_byte_budget Check if this will lead to exceeding table budget.
#' @export
adjust_view <- function(view, max_tries = 5L, check_byte_budget = TRUE) {

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
    if(!length(reason)) stop(glue::glue("Server returned response that can't be handled: {msg}"))

    if(grepl("too small", reason)) {
      view <- adjust_size(view, hint = reason)
      attempts <- attempts + 1
    }

    if(grepl("maximumListLength", reason)) {
      view <- adjust_list_length(view, hint = reason)
      attempts <- attempts + 1
    }
  }
}


#' Adjust schema max size based on hint
#'
#' Note: For STRING cols, the hard limit is 1000 char size,
#' though at 250 using LARGETEXT is officially recommended, so possibly at that breakpoint
#' this should just create the different column type instead of increasing size.
#'
#' @keywords internal
adjust_string_size <- function(view, hint, check_byte_budget = TRUE) {

    # parse to extract column and resizing recommendation
    col_name <- regmatches(hint, regexpr("(?<=\')(.*?)(?=\')", hint, perl = TRUE))
    size <-  regmatches(hint, regexpr("[0-9]+(?= characters)", hint, perl = TRUE))
    # convert warn to error if this is not a num
    size <- tryCatch(as.integer(size), warning = function(w) stop("Unable to parse size from ", hint))
    message(glue::glue("adjusting size for '{col_name}'..."))

    # get schema and apply changes; must create new immutable column with diff size
    schema <- .syn$get(view)
    new_col <- ref_col <- match_col(schema, col_name)
    new_col$id <- NULL
    new_col$maximumSize <- size
    if(check_byte_budget) check_byte_budget_col_swap(schema, ref_col, new_col) # errors and will not proceed to create new col if fail
    new_col <- new_col(new_col)

    # then update schema
    schema <- swap_col(schema, old = ref_col$id, new = new_col$id)

    return(schema$properties$id)
}


#' Adjust schema max list length based on hint
#'
#' @keywords internal
adjust_list_length <- function(view, hint, check_byte_budget = TRUE) {

  # parse to extract column length recommendation
  col_name <- regmatches(hint, regexpr("(?<=\")(.*?)(?=\")", hint, perl = TRUE))
  len <-  regmatches(hint, regexpr("[0-9]+$", hint, perl = TRUE))
  len <- tryCatch(as.integer(len), warning = function(w) stop("Unable to parse length from ", hint))
  message(glue::glue("adjusting list length for '{col_name}'..."))

  # get schema and apply changes; new col def with diff maximumListLength
  schema <- .syn$get(view)
  new_col <- ref_col <- match_col(schema, col_name)
  new_col$id <- NULL
  new_col$maximumListLength <- len
  if(check_byte_budget) check_byte_budget_col_swap(schema, ref_col, new_col)
  new_col <- new_col(new_col)

  schema <- swap_col(schema, old = ref_col$id, new = new_col$id)

  return(schema$properties$id)

}

#- Helpers ---------------------------------------------------------------------#

#' Check byte budget when swapping cols in schema
#'
#' @keywords internal
check_byte_budget_col_swap <- function(schema, ref_col, new_col) {
  adj_remaining <- byte_budget(schema) + byte_budget(schema_cols = list(ref_col), result = "allocated")
  req_allocate <- byte_budget(new_col)
  if(req_allocate > adj_remaining) stop(glue::glue("Will have {adj_remaining} bytes in table width budget but adjustments require {req_allocate} bytes"))
}

#' Find matching col in schema based on name
#'
#' Synapse doesn't allow schemas to have columns of same name; this should never return more than one.
#' @keywords internal
match_col <- function(schema, col_name) {

  schema_cols <- .syn$getTableColumns(schema) %>% reticulate::iterate()
  index <- match(col_name, sapply(schema_cols, `[[`, "name"))
  if(is.na(index)) stop("Encountered issue finding relevant column in schema")
  ref_col <- schema_cols[[index]]
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
#' @param new Id of new col.
#' @keywords internal
swap_col <- function(schema, old, new) {

  schema$removeColumn(old)
  schema$addColumn(new)
  schema <- .syn$store(schema)
  message(glue::glue("Updated schema to replace old column {old} with new column {new}"))
  schema
}


#' Calculate byte budget for a schema
#'
#' Tables have a hard width limit of 64KB.
#' Given a current table schema, this does math for how many bytes remain or are already allocated.
#' Useful as an austerity measure if indeed one has a very large table,
#' or in other cases when philosophically being more principled in schema configuration.
#'
#' See also:
#' - https://rest-docs.synapse.org/rest/org/sagebionetworks/repo/model/table/ColumnType.html
#'
#' @param table Existing Synapse table id or the table schema object used to retrieve the column types.
#' @param schema_cols Optional, this also can take a list of column characteristics; use when building from scratch and columns are not yet stored.
#' If given, `table` will be ignored.
#' @param result Return the summary number for "remaining" or "allocated", or return a TRUE/FALSE for "within" budget.
#' @export
byte_budget <- function(table, schema_cols = NULL, result = "remaining") {

  if(is.null(schema_cols)) schema_cols <- .syn$getTableColumns(table) %>% reticulate::iterate()

  WIDTH_LIMIT <- 64000

  # bytes for col type
  STRING <- expression( 4L * maximumSize )
  DOUBLE <- 23L
  INTEGER <- 20L
  BOOLEAN <- 5L
  DATE <- 20L
  FILEHANDLEID <- 20L
  ENTITYID <- 44L
  SUBMISSIONID <- 20L
  EVALUATIONID <- 20L
  LINK <- expression( 4L * maximumSize )
  MEDIUMTEXT <- 421L
  LARGETEXT <- 2133L
  USERID <- 20L
  STRING_LIST <- expression( 4L * maximumSize * maximumListLength )
  INTEGER_LIST <- expression( 20L * maximumListLength )
  BOOLEAN_LIST <-  expression( 5L * maximumListLength )
  DATE_LIST <- expression( 20L * maximumListLength )
  ENTITYID_LIST <- expression( 44L * maximumListLength )
  USERID_LIST <- expression( 20L * maximumListLength )
  JSON <- 2133L

  allocated <- sum(sapply(schema_cols, function(x) {
    eval(get(x$columnType), envir = x) }
  ))

  if(result == "remaining") {
    WIDTH_LIMIT - allocated
  } else if(result == "allocated") {
    allocated
  } else if(result == "within") {
    allocated < WIDTH_LIMIT
  } else {
    NA
  }
}

