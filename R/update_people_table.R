#' Update the People table from a source Table or View column
#'
#' @param people_table_id The synapse id of the table used for referencing people.
#' @param people_column Column name within the people table that contains the relevant people values.
#' @param source_table_id The synapse id of the source table.
#' @param source_column Column name within the source table that contains the relevant source values.
#' @inheritParams update_study_annotations
#' @return If dry_run == T, prints preview of updated people table, otherwise uploads the updates.
#' @export
#'
add_people_from_table <- function(people_table_id, people_column, source_table_id, source_column, dry_run = T){

  .check_login()

  #get current people ids
  old_people_query <- glue::glue("select {people_column} from {people_table_id}")

  old_people <- synapser::synTableQuery(old_people_query, includeRowIdAndRowVersion	= F)$asDataFrame() %>%
    tibble::as_tibble() %>%
    dplyr::distinct()

  old_ids <- old_people[[{{people_column}}]]

  #get ids from new source table
  new_people_query <- glue::glue("select {source_column} from {source_table_id}")

  new_people <- synapser::synTableQuery(new_people_query, includeRowIdAndRowVersion	= F)$asDataFrame() %>%
    tibble::as_tibble() %>%
    dplyr::distinct() %>%
    dplyr::filter(!(!!rlang::sym(source_column)) %in% old_ids) %>%   #filter out existing ids
    purrr::pluck(source_column)

  message(glue::glue("adding {length(new_people)} new people"))

  new_rows <- tibble::tibble(!!people_column := new_people)

  schema <- synapser::synGet(people_table_id)

  if(dry_run == F){
    .store_rows(schema, new_rows)
  }else{
    new_rows
  }

}







