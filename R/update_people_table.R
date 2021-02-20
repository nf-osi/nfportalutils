#' Update the People table from a source Table or View column
#' @description
#' @param fileview_id The synapse id of a fileview. Must have the desired annotations in the schema, and must have the files to annotate included in the scope. Must have write access to the files you want to re-annotate.
#' @param annotation_key A character string of the annotation you'd like to switch from a delimited string to a stringlist.
#' @param sep Default = ",". The delimiter in the character string.
#' @param trimws Default = TRUE. Remove white space at the beginning and end of list items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST result).
#' @param dry_run Default = TRUE. Skips upload to table and instead prints study tibble.
#' @return If dry_run == T, returns study tibble and skips upload.
#' @export
#'
add_people_from_table <- function(people_table_id, people_column, source_table_id, source_column, dry_run = T){

  .check_login()

  #get current people ids
  old_people_query <- glue::glue("select {people_column} from {people_table_id}")

  old_people <- .syn$tableQuery(old_people_query)$asDataFrame() %>%
    dplyr::distinct()

  old_ids <- old_people[[{{people_column}}]]

  #get ids from new source table
  new_people_query <- glue::glue("select {source_column} from {source_table_id}")

  new_people <- .syn$tableQuery(new_people_query)$asDataFrame() %>%
    dplyr::distinct() %>%
    dplyr::filter(!(!!rlang::sym(source_column)) %in% old_ids) %>%   #filter out existing ids
    purrr::pluck(source_column)

  message(glue::glue("adding {length(new_people)} new people"))

  new_rows <- tibble::tibble(!!people_column := new_people)

  schema <- .syn$get(people_table_id)

  if(dry_run == F){
    .store_rows(schema, new_rows)
  }else{
    new_rows
  }

}







