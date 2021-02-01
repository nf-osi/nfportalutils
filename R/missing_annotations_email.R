#' Convert a delimited string to a stringlist annotation
#' @description Converts a delimited string to a stringlist annotation and adjust the associated schema in the portal fileview.
#' @param fileview_id The synapse id of a fileview. Must have the desired annotations in the schema, and must have the files to annotate included in the scope. Must have write access to the files you want to re-annotate.
#' @param annotation_key A character string of the annotation you'd like to use to detect unannotated files.
#' @param created_date The date ('DD/MM/YYYY') to cut off
#' @param dry_run Default = TRUE. Skips emailing and instead prints summary tibble.
#' @return If dry_run == T, returns study tibble and skips upload.
#' @export

##TODO: finish this function

missing_annotation_email <- function(fileview_id, annotation_key, created_date, dry_run = TRUE){

  .check_login()

  lubridate::as_date(created_date)

  fv <- .syn$tableQuery(glue::glue('select id, name, {annotation_key}, createdOn, createdBy from {fileview_id} where type = \'file\' and {annotation_key} is NULL'))$filepath %>%
    readr::read_csv(na=character()) ##asDataFrame() & reticulate return rowIdAndRowVersion as concatenated rownames, read_csv reads them in as column

  fv_nest <- fv %>%
    dplyr::group_by(createdBy) %>%
    tidyr::nest()
}
