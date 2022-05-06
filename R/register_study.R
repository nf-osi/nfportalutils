#' Register a study in Portal - Studies
#' 
#' Basically create a row for a new NF-OSI project study in Portal - Studies table
#' 
#' @inheritParams new_project
#' @param project_id The Synapse project id, used as the studyId.
#' @param focus NF focus, e.g. "Neurofibromatosis type 1".
#' @param manifestation Character vector of enum NF manifestation(s) associated with study.
#' @param fileview_id Synapse id of the study project's main fileview.
#' @param study_status Status of study, defaults to "Active" for new projects.
#' @param data_status Status of the data, defaults to "None" for new projects.
#' @param terms_access Access requirements blurb, uses a default for new projects.
#' @param terms_acknowledgement Blurb for how study should be acknowledged if materials are reused. 
#' Unless specified by PI/leads at inception, leave blank for new projects and update later.
#' @param study_table_id The Synapse id of the portal study table. 
#' @param grant_doi DOI of grant proposal if available.
#' @export
register_study <- function(name,
                           project_id, 
                           abstract, 
                           lead, 
                           institution, 
                           focus, 
                           manifestation,
                           fileview_id,
                           funder = c("CTF", "GFF", "NTAP"),
                           initiative,
                           study_status = "Active",
                           data_status = "None",
                           terms_access = "The data from this study is currently under embargo. Please contact the principal investigator for access to the data.",
                           terms_acknowledgement = 'The data from this study are still under embargo, therefore, if you have been granted access by the data contributor, you must work with them to determine how to acknowledge your collaboration in any manuscripts that arise. In addition, please acknowledge the NF Data Portal like so: "The results published here are in whole or in part based on data obtained from the NF Data Portal (http://www.nf.synapse.org, RRID:SCR_021683) and made available through the NF Open Science Initiative."',
                           grant_doi = "",
                           study_table_id = "syn16787123") {
  
  schema <- .syn$get(study_table_id)
  # Basic validation for project_id and fileview_id where expect format "syn12345678" (allow 9 digits eventually?)
  if(!grepl("^syn[0-9]{8}", project_id)) stop("Possible typo/missing something in project Synapse id?")
  if(!grepl("^syn[0-9]{8}", fileview_id)) stop("Possible typo/missing something in fileview Synapse id?")
  
  new_row <- data.frame(studyName = name,
                       studyId = project_id,
                       summary = abstract,                   
                       initiative = initiative,                
                       studyLeads = strlist_JSON(lead), # STRLIST                
                       institutions = strlist_JSON(institution, sep = "; ?"),  # STRLIST  
                       manifestation = strlist_JSON(manifestation),  # STRLIST  
                       diseaseFocus = strlist_JSON(focus),  # STRLIST  
                       studyStatus = study_status,
                       dataStatus = data_status,
                       fundingAgency = strlist_JSON(funder),  # STRLIST  
                       accessRequirements = terms_access,
                       acknowledgementStatements = terms_acknowledgement,
                       dataType = "", # STRLIST  
                       relatedStudies = "", # STRLIST  
                       studyFileviewId = fileview_id,
                       id = project_id,
                       grantDOI = strlist_JSON(grant_doi), # STRLIST                   
                       Resource_id = "") # STRLIST  
  .syn$store(synapseclient$Table(schema, new_row))
  
}


#' Add new project scope to "Portal - Files" fileview
#' 
#' Add a new project to the scope of the "Portal - Files" fileview so that 
#' files for that project are "registered" and surfaced in portal.
#' 
#' @param project_id The project id, i.e. container, that will be added to the scope of the view.
#' @param portal_fileview Synapse id of "Portal - Files" entity view.
#' @export
register_study_files <- function(project_id, 
                                 portal_fileview = "syn16858331") {
  
  new_scope_id <- sub("syn", "", project_id)
  portal_fileview <- .syn$get(portal_fileview)
  # Works, but maybe there's a better implementation?
  reticulate::py_set_attr(portal_fileview, "scopeIds", c(portal_fileview$properties$scopeIds, new_scope_id))
  .syn$store(portal_fileview)
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

