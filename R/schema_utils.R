# Util functions for playing nice with SYNAPSE schemas and SCHEMATIC json-ld schemas

# -- Synapse schema ------------------------------------------------------------#

#' Transform table data to target schema for Synapse storage 
#' 
#' **Currently implements list-schema features first and will do more later.** 
#' Check and encode data values to expectations of Synapse target table schema for storage. 
#' The target schema is more likely from an existing table, since new tables can take advantage of `build_table`. 
#' To get compatible list data, does JSON encoding and optionally `list_truncate` when running into length limits.
#' If truncation is not OK, then the incompatibility will have to be resolved by updating schema outside of this. 
#' Note that the setting applies to ALL list columns, though it would be desirable to be column-specific.
#' 
#' @param df A table, i.e. `data.frame`.
#' @param schema Table [schema object](https://python-docs.synapse.org/build/html/Entity.html#synapseclient.table.Schema) or 
#' Synapse id of target table from which to get schema.
#' @param list_truncate If length exceeds schema max for list columns, set `TRUE` to allow data truncation, `FALSE` to error only (default).
#' @return Synapse Table object ready for storing.
as_table_schema <- function(df, 
                            schema, 
                            list_truncate = FALSE) {
  
  if("synapseclient.table.Schema" %in% class(schema) && reticulate::py_has_attr(schema, "columns_to_store")) { 
    col_schema <- schema$columns_to_store
  } else {
    if(is.character(schema)) schema <- .syn$get(schema)
    col_schema <- .syn$getTableColumns(schema) %>% reticulate::iterate()
  }
  
  # Basic checks of columns
  col_schema_names <- sapply(col_schema, `[[`, "name")
  if(length(col_schema_names) != length(df)) stop("Number of columns differs from schema.")
  tryCatch({
    df <- df[col_schema_names] # enforce same order as schema while checking names
  }, error = function(e) stop("Column names don't match ones in schema."))
  
  # https://docs.synapse.org/rest/org/sagebionetworks/repo/model/table/ColumnType.html
  col_type <- sapply(col_schema, `[[`, "columnType")
  for(i in seq_along(col_type)) {
    values <- df[[i]]
    if(grepl("STRING", col_type[i])) {
      maxsize <-  col_schema[[i]]$maximumSize
      size_fail <- sapply(values, function(x) any(nchar(x) > maxsize))
      if(any(size_fail)) stop(paste("Characters in", names(df)[i], "exceeds max size of", maxlen))
    }
    if(grepl("*_LIST", col_type[i])) {
      maxlen <- col_schema[[i]]$maximumListLength
      len_fail <- sapply(values, function(x) length(x) > maxlen)
      if(any(len_fail)) {
        if(list_truncate) {
          values[len_fail] <- sapply(values[len_fail], function(x) x[1:maxlen])
          warning(paste("Data truncated for", names(df)[i], "to fit max list length of", maxlen))
        } else {
          stop(paste("Data in", names(df)[i], "exceeds max list length of", maxlen))
        }
      }
      df[[i]] <- sapply(values, function(x) as.character(jsonlite::toJSON(unlist(x)))) # unlist in case x is derived from list
    } 
  }
  table_data <- synapseclient$Table(schema, df)
  table_data
}


