#' Register a NEW project for the NF Data Portal in **Portal - Project View**
#'
#' Add relevant study metadata to the project as annotations.
#' Add to scope of NF-OSI data portal and management in **Portal - Project View**.
#'
#' @inheritParams new_project
#' @param id Synapse id.
#' @export
register_study <- function(id,
                           study_meta,
                           portal_project_view = "syn*") {

    add_new_study_meta(id, study_meta)
    add_study_summary(id, description)
    add_to_scope(portal_project_view, project_id)
}


#' Register a project's files in **Portal - Files**
#'
#' Add a project to the scope of the **Portal - Files** fileview so that
#' files for that project are "registered" and surfaced in portal.
#'
#' @param project_id The project id, i.e. container, that will be added to the scope of the view.
#' @param portal_fileview Synapse id of "Portal - Files" entity view.
#' @export
register_study_files <- function(project_id,
                                 portal_fileview = "syn16858331") {

  add_to_scope(portal_fileview, project_id)
}


#' Helpers ---------------------------------------------------------------------#

#' New and selective addition of study meta as annotations
#'
#' Put *selected* metadata into Synapse annotations for the study project entity.
#'
#' @param id Id of container representing the study, usually a Synapse project.
#' @param study_meta Study meta as a list.
#' @keywords internal
add_new_study_meta <- function(id, study_meta) {

  study_meta <- study_meta[c(
    "studyName",
    "fundingAgency",
    "initiative",
    "studyLeads",
    "institutions",
    "diseaseFocus",
    "manifestation",
    "studyStatus",
    "dataStatus",
    "grantDOI",
    "relatedStudies",
    "studyFileviewId")]

  if(is.null(study_meta$studyStatus)) study_meta$studyStatus <- "Active"
  if(is.null(study_meta$dataStatus)) study_meta$dataStatus <- "Data Pending"

  study <- .syn$setAnnotations(id, study_meta)
  invisible(study)
}

#' Add study summary to a table
#'
#' @param study_id Synapse id.
#' @param table_d Table id of where to store summary.
add_study_summary <- function(study_id, summary, table_id = "syn16787123") {

}


#' Add to scope
#'
#' Conveniene function to add container to view scope.
#' @export
add_to_scope <- function(view, new_id) {

  view <- .syn$get(view)
  new_scope_id <- sub("syn", "", new_id) # should be integer id
  view$add_scope(new_scope_id)
  view <- .syn$store(view)
  invisible(view)
}

#' Convert delimited record to JSON representation needed by a stringlist col schema
#'
#' Internal helper that reuses and extends the utility of `.delim_string_to_vector`.
#'
#' @inheritParams .delim_string_to_vector
#' @param record Character vector of length one representing a single record.
#' @keywords internal
strlist_JSON <- function(record, sep = ",", trim_ws = T) {
  .delim_string_to_vector(string = record, sep, trim_ws = T) %>%
    jsonlite::toJSON() %>%
    as.character()
}

