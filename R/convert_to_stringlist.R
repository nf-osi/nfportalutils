#' Convert a delimited string to a stringlist annotation
#' @description Converts a delimited string to a stringlist annotation and adjust the associated schema in the portal fileview.
#' @param fileview_id The synapse id of a fileview. Must have the desired annotations in the schema, and must have the files to annotate included in the scope. Must have write access to the files you want to re-annotate.
#' @param annotation_key A character string of the annotation you'd like to switch from a delimited string to a stringlist.
#' @param sep Default = ",". The delimiter in the character string.
#' @param trimws Default = TRUE. Remove white space at the beginning and end of list items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST result).
#' @param dry_run Default = TRUE. Skips upload to table and instead prints study tibble.
#' @return If dry_run == T, returns study tibble and skips upload.
#' @export
#'
convert_to_stringlist <- function(fileview_id, annotation_key, sep = ",", trim_ws = TRUE, dry_run = TRUE){

  .check_login()

  fv <- .syn$tableQuery(glue::glue('select id,{annotation_key} from {fileview_id} where type = \'file\''))$filepath %>%
    readr::read_csv(na=character()) ##asDataFrame() & reticulate return rowIdAndRowVersion as concatenated rownames, read_csv reads them in as column

  fv_filt <- dplyr::filter(fv, !!rlang::sym(annotation_key) != "") %>%
    dplyr::filter(grepl(sep,!!rlang::sym(annotation_key)))

  ls_annotations <- lapply(fv_filt$id, function(x){
    fv_filt[fv_filt$id==x,annotation_key] %>% purrr::pluck(annotation_key) %>% .delim_string_to_vector(., trim_ws = T, sep = sep)
    })

  names(ls_annotations) <- fv_filt$id

  max_str_len <- lapply(ls_annotations, stringr::str_length) %>% unlist %>% max

  if(dry_run == F){
    lapply(names(ls_annotations), function(x){
      .modify_annotation(synapse_id = x, key = annotation_key, value = ls_annotations[[x]])
    })

    .replace_string_column_with_stringlist_column(table_id = fileview_id, column_name = annotation_key)
  }else{
    message(glue::glue("{length(ls_annotations)} files will be updated when dry_run = F."))
  }
}

#' Convert a delimited string to vector, utility function.
#' @description Converts a delimited string to a stringlist annotation and adjust the associated schema in the portal fileview.
#' @param string A character string.
#' @param sep Default = ",". The delimiter in the character string.
#' @param trimws Default = TRUE. Remove white space at the beginning and end of list items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST result).
#' @export
#'
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

#' Modify a single annotation on a single file.
#' @description Modifies a single annotation value on a single (existing) synapse file. Guts of this ripped from @jaeddy gist (https://gist.github.com/jaeddy/1cf49f7851945beedb39d431134734af)
#' @param synapse_id A synapse entity id.
#' @param key The key of the annotation to modify.
#' @param value The value to change the annotation to.
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
