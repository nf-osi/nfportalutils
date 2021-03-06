#' Summarize file annotations into a STRINGLIST column on a study table.
#' @description Summarize fileview annotations into a string-list column on another table.
#' @description For example, use this function to summarize all of the "dataType" annotations for a each study into a STRINGLIST annotation on the Study table of a portal. Overwrites whatever is currently in the target column.
#' @param study_table_id The synapse id of the portal study table. Must have write access.
#' @param fileview The Synapse ID of the portal fileview.
#' @param group_colname The column name to group by and join on (such as the default = 'studyId')
#' @param source_colname The column name to summarize and add to the study_table_id table. The column must exist in both schemas, and must be a STRINGLIST-type column in the "study_table_id" table.
#' @param sep If any delimited values exist in the source_colname column, pass the delimiter here so that these cases are included.
#' @param valid_values A vector of valid values for the source_colname. e.g. the output of running `get_valid_values_from_json_schema()`
#' @param dry_run Default = TRUE. Skips upload to table and instead prints study tibble.
#' @return If dry_run == T, returns study tibble and skips upload.
#' @example assign_study_data_types(study_table_id = 'syn16787123', fileview_id = 'syn16858331', group_colname = 'studyId', source_colname = "dataType", sep = ",", valid_values = get_valid_values_from_json_schema(), dry_run = T)
#' @export
assign_study_data_types <- function(study_table_id, fileview_id, group_colname = "studyId",
                                    source_colname = "dataType", sep = ",", valid_values, dry_run = TRUE){

  .check_login()

  ##query the study table
  query <- .syn$tableQuery(glue::glue("select {group_colname}, {source_colname} from {study_table_id}", includeRowIdAndRowVersion=T))

  studies <- query$filepath %>%
    readr::read_csv(na=character()) ##asDataFrame() & reticulate return rowIdAndRowVersion as concatenated rownames, read_csv reads them in as columns

  ##query the fileview
  fv <- .syn$tableQuery(glue::glue('select {group_colname},{source_colname} from {fileview_id} where type = \'file\' and {group_colname} is not null and {source_colname} is not null'))$filepath %>%
    readr::read_csv(na=character()) ##asDataFrame() & reticulate return rowIdAndRowVersion as concatenated rownames, read_csv reads them in as column

  #TODO:: add support for stringlist-ed values

  ##make simplified data table for stringlist-ing
  data_types <- fv %>%
    dplyr::select(one_of({{group_colname}}, {{source_colname}})) %>%
    dplyr::distinct() %>%
    tidyr::separate_rows({{source_colname}}, sep = {{sep}}) %>% ##this handles comma seperated or other delimited values
    dplyr::filter(!!rlang::sym(source_colname) %in% valid_values)

  studies <- dplyr::select(studies, ROW_ID, ROW_VERSION, {{group_colname}})

  ##create stringlisted data
  ids <- data_types %>% dplyr::group_by_at(group_colname) %>%
    dplyr::summarise(!!rlang::sym(source_colname) := jsonlite::toJSON(!!rlang::sym(source_colname)))

  ##join study table to stringlisted values, filter NA rows out, we don't need to update those
  studies_updated <- dplyr::left_join(studies, ids) %>%
    dplyr::filter(!is.na(!!rlang::sym(source_colname)))

  #TODO: could add check here to report number of updated rows vs original...

  if(dry_run == FALSE){
    .update_table_data(table_id = study_table_id,
                       new_data = studies_updated,
                       etag = query$etag)
  }else{
    studies_updated
  }
}

#' Retrieve valid subclasses of a value in a JSON-LD schema
#' @description Retrieve valid subclasses of a value in a JSON-LD schema generated by schematic.
#' @param schema_url Default: the NF-OSI JSON-LD schema.
#' @param parent_name Default = DataType. The value for which you'd like to find the associated subclasses.
#' @param parent_context Default = bts. The JSON-LD context for the value in question.
#' @return A character vector of values.
#' @export
get_valid_values_from_json_schema <- function(schema_url = 'https://raw.githubusercontent.com/nf-osi/schematic/develop/data/schema_org_schemas/NF.jsonld',
                                              parent_name = 'DataType',
                                              parent_context = 'bts'){

  parent_id <- paste0(parent_context, ':', parent_name)
  
  subclasses <- 
    jsonlite::fromJSON(schema_url) %>%
    purrr::pluck("@graph") %>%
    dplyr::filter(purrr::map_lgl(`rdfs:subClassOf`, ~ parent_id %in% .x)) %>% 
    dplyr::pull(`sms:displayName`)

  subclasses
}
