#' Updates a set of files with project-level annotations.
#' @description Adds studyId, studyName, initiative, funder annotations to a set of files from a fileview query. Assumes that projectId==studyId. You must have the Synapse default view column "type" present in your view. After running this function, changes may take a few minutes to propagate through Synapse.
#' @param study_table_id The synapse id of the portal study table.
#' @param fileview_id The synapse id of a fileview. Must have the desired annotations in the schema, and must have the files to annotate included in the scope. Must have write access to the files you want to re-annotate.
#' @param annotations A vector of annotations to gather from the study table, and assign to the files.
#' @param dry_run Default = TRUE Skips upload of annotations unless set to FALSE.
#' @return If dry_run == T, returns updated annotations as a tibble.
#' @examples 
#' \dontrun{
#' update_study_annotations(study_table_id = "syn16787123",
#'                          fileview_id = "syn16858331",
#'                          annotations = c("studyId","studyName","initiative","fundingAgency"),
#'                          dry_run = T)
#' }
#' @export
#'
update_study_annotations <- function(study_table_id, fileview_id, annotations, dry_run = T){
  
  .check_login()
  
  fv <- .syn$tableQuery(glue::glue('select {glue::glue_collapse(annotations, sep = ",")},projectId from {fileview_id} where type = \'file\''))$filepath %>%
    readr::read_csv(na=character()) ##asDataFrame() & reticulate return rowIdAndRowVersion as concatenated rownames, read_csv reads them in as column
  
  study_table <- .syn$tableQuery(glue::glue('select {glue::glue_collapse(annotations, sep = ",")} from {study_table_id}'),
                                 includeRowIdAndRowVersion = F)$filepath %>%
    readr::read_csv(na=character())
  
  ##temporary change to make backwards compatible with non-stringlisted annotations.
  ##once portal annotations are switched to stringlist in all applicable places, can remove this.
  if('fundingAgency' %in% annotations){
    study_table$fundingAgency <- sapply(study_table$fundingAgency, function(x){
      jsonlite::parse_json(x) %>% unlist %>% glue::glue_collapse(sep = ",")
      })
  }
  
  fv_updated <- fv %>%
    dplyr::select(ROW_ID, ROW_VERSION, ROW_ETAG, projectId) %>%
    dplyr::mutate(studyId = projectId) %>%
    dplyr::left_join(study_table, by = "studyId") %>%
    dplyr::filter_at(dplyr::all_of(annotations), dplyr::all_vars(!is.na(.))) %>%
    dplyr::select(dplyr::any_of(colnames(fv)))
  
  skipped <- fv$ROW_ID[!fv$ROW_ID %in% fv_updated$ROW_ID]
  
  if(length(skipped)!=0){
    message(glue::glue("Some metadata unavailable from study table. Skipping {length(skipped)} files."))
  }
  
  updates <- dplyr::setdiff(fv_updated, fv)
  
  if(nrow(updates)>0 & dry_run == F){
    
    .update_view_data(fileview_id, updates)
    
    message(glue::glue("Annotations for {nrow(updates)} files updated."))
    
  }else if(dry_run == T){
    
    updates
    
  }else{
    
    message("Annotations are already up-to-date.")
    
  }
  
  
}

#' Replace/update table contents = input data must have ROW_ID, ROW_VERSION, ETAG columns to update.
#' @param table_id The synapse id of the table to update.
#' @param new_data The updated table.
#' @export
.update_view_data <- function(table_id, new_data){
  table <- .syn$store(synapseclient$Table(table_id, new_data))
  #TODO: add "in progress" indicator
}
