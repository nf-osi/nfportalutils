#' Create a new project
#' 
#' @description Set up a new NF project with wiki, folders, fileview, and permissions.
#' Most parameters come from a project intake & data sharing plan (DSP) form.
#' Aside from default folders, folders are tailored for data mentioned in DSP. 
#' The NF-OSI team is hard-coded to be admin in addition to the funder team indicated by `funder`. 
#' 
#' After project is created, NF Portal representation requires registration in backend:
#' - New study row added to the Portal - Studies table.
#' - Project added to Portal - Files scope.
#' 
#' @param name Name of the project/study.
#' @param pi Name of the principal investigator.
#' @param lead Name(s) of the project lead/data coordinator, comma-sep if multiple, e.g. "Jane Doe, John Doe".
#' @param admin_user (Optional) Synapse username of specified user to be made admin. 
#' Currently, only takes one admin user, and the rest can be added via the UI. 
#' @param abstract Project abstract/description.
#' @param institution Affiliated institution(s), **semicolon-sep if multiple**, e.g. "Stanford University; University of California, San Francisco".
#' @param funder The funder org, currently one of c("CTF", "GFF", "NTAP"). The relevant funder team will be made admin.
#' @param initiative Title of funding initiative, e.g. "Young Investigator Award".
#' @param webview Whether to open web browser to view newly created project. Defaults to FALSE.
#' @return The project object.
new_project <- function(name, 
                        pi,
                        lead,
                        admin_user = NULL,
                        abstract, 
                        institution,
                        funder = c("CTF", "GFF", "NTAP"), 
                        initiative,
                        webview = FALSE) {
  
  .check_login()

  project <- synapseclient$Project(name)
  project <- .syn$store(project)
  
  # WIKI -----------------------------------------------------------------------#
  
  content <- glue::glue("
  # {name}
  ## {funder} - {initiative}
  ### Principal Investigator: {pi}
  ### Project Lead / Data Coordinator: {lead}
  ### Institution: {institution}
  ### Project Description: 
  {abstract}
  ")
  
  wiki <- synapseclient$Wiki(owner = project,
                       title = name,
                       markdown = content)
  
  # Push wiki to Synapse
  wiki <- .syn$store(wiki)
  
  # Add a subpage w/ links to the Data Curator App as of Dec 2021
  wiki <- data_curator_app_subpage(project_id = project, dry_run = FALSE)
  
  # PERMISSIONS ----------------------------------------------------------------#
  # Set NF-OSI Sage Team permissions to full admin
  NF_sharing <- make_admin(project, principal_id = 3378999)
  
  # Set grant funding team to full admin
  funder <- match.arg(funder)
  funder <- switch(funder,
                   CTF = 3359657, ##CTF team
                   GFF = 3406072, ##GFF Admin team
                   NTAP = 3331266) ##NTAP Admin team
  funder_sharing <- make_admin(project, funder)
  
  # Set project lead/pi user to full admin user if given
  if(!is.null(admin_user)) {
    user_sharing <- make_admin(project, admin_user)
  }
  
  # ASSETS ---------------------------------------------------------------------#
  # Create default upper-level folders
  add_default_folders(project)
  
  # Create data-specific folders in "Raw Data"
  # make_folder(parent = data_folder$properties$id, folders = c(""))
  
  # Add Project Files and Metadata fileview, add NF schema; currently doesn't add facets
  view <- synapseclient$EntityViewSchema(name="Project Files and Metadata",
                                    columns=list(
                                     synapseclient$Column(name="assay", columnType="STRING", maximumSize="57"),
                                     synapseclient$Column(name="consortium", columnType="STRING", maximumSize="24"),
                                     synapseclient$Column(name="dataSubtype", columnType="STRING", maximumSize="13"),
                                     synapseclient$Column(name="dataType", columnType="STRING", maximumSize="30"),
                                     synapseclient$Column(name="diagnosis", columnType="STRING", maximumSize="39"),
                                     synapseclient$Column(name="tumorType", columnType="STRING", maximumSize="90"),
                                     synapseclient$Column(name="fileFormat", columnType="STRING", maximumSize="13"),
                                     synapseclient$Column(name="fundingAgency", columnType="STRING", maximumSize="12"),
                                     synapseclient$Column(name="individualID", columnType="STRING", maximumSize="213"),
                                     synapseclient$Column(name="nf1Genotype", columnType="STRING", maximumSize="8"),
                                     synapseclient$Column(name="nf2Genotype", columnType="STRING", maximumSize="7"),
                                     synapseclient$Column(name="species", columnType="STRING", maximumSize="15"),
                                     synapseclient$Column(name="resourceType", columnType="STRING", maximumSize="50"),
                                     synapseclient$Column(name="isCellLine", columnType="STRING", maximumSize="50"),
                                     synapseclient$Column(name="isMultiSpecimen", columnType="STRING", maximumSize="50"),
                                     synapseclient$Column(name="isMultiIndividual", columnType="STRING", maximumSize="50"),
                                     synapseclient$Column(name="studyId", columnType="ENTITYID"),
                                     synapseclient$Column(name="studyName", columnType="LARGETEXT"),
                                     synapseclient$Column(name="specimenID", columnType="STRING", maximumSize="300"),
                                     synapseclient$Column(name="sex", columnType="STRING", maximumSize="50"),
                                     synapseclient$Column(name="age", columnType="STRING", maximumSize="50"),
                                     synapseclient$Column(name="readPair", columnType="INTEGER"),
                                     synapseclient$Column(name="progressReportNumber", columnType="INTEGER"),
                                     synapseclient$Column(name="accessType", columnType="STRING", maximumSize="50"),
                                     synapseclient$Column(name="accessTeam", columnType="USERID"),
                                     synapseclient$Column(name="cellType", columnType="STRING", maximumSize="300"),
                                     synapseclient$Column(name="modelOf", columnType="STRING", maximumSize="50"),
                                     synapseclient$Column(name="compoundName", columnType="STRING", maximumSize="156"),
                                     synapseclient$Column(name="experimentalCondition", columnType="STRING", maximumSize="58"),
                                     synapseclient$Column(name="modelSystemName", columnType="STRING", maximumSize="42"),
                                     synapseclient$Column(name="isXenograft", columnType="STRING", maximumSize="5"),
                                     synapseclient$Column(name="transplantationType", columnType="STRING", maximumSize="50")),
                                   parent=project,
                                   scopes=project,
                                   includeEntityTypes=list(synapseclient$EntityViewType$FILE),
                                   add_default_columns=TRUE)
  view <- .syn$store(view)
  
  if(webview) .syn$onweb(project)
  #TODO: add snippet to add the project to the Portal Files view scope
  #TODO: add snippet to add the project to the Portal Studies table as a row
  return(project)
}

#' Make a user or group full admin of a Synapse entity
#' 
#' Convenience method to set admin permissions
make_admin <- function(entity, principal_id) {
  admin <- .syn$setPermissions(entity = entity,
                               principalId = principal_id,
                               accessType = list('DELETE', 'CHANGE_SETTINGS', 'MODERATE', 'CREATE', 'READ','DOWNLOAD', 'UPDATE', 'CHANGE_PERMISSIONS'))
}

#' Create project folders
#' 
#' Use to set up a scaffold of standard upper-level folders as well as
#' customized data folders within "Raw Data" for a new project.
#' @param parent The Synapse id or object that should be the parent container, i.e. the project or another folder. 
#' @param folders Character vector giving one or more folder names of folder(s) to create.
#' @return A list of the created folder object(s).
make_folder <- function(parent, folders) {
  
  refs <- list()
  for (i in folders) {
    folder <- synapseclient$Folder(i, parent = parent)
    folder <- .syn$store(folder)
    refs[[i]] <- folder
  }
  return(refs)
}

#' Create default folders
#' 
#' A convenience wrapper around `make_folder` with NF defaults.
#' @param project The project Synapse id or object.
add_default_folders <- function(project, folders = c("Analysis", "Milestone Reports", "Raw Data")) {
   make_folder(parent = project, folders) 
}

#' Get and parse data from Google Sheets for initializing a new project
#' 
#' Currently, project tracking data is stored in a private GoogleSheet.
#' For \link{\code{new_project}}, this wraps `googlesheets4` to get the needed data.
#' 
#' If `creds` is not provided, i.e. there is no service account token,
#' then usage requires authenticating the Tidyverse API in the browser to get an API token on your behalf.
#' This doesn't actually give the googlesheets4 project access to any data
#' ("The Tidyverse API Packages project never receives your data or the permission to access your data."
#' -- see https://www.tidyverse.org/google_privacy_policy/.)  
#' 
#' @param sheet Sheet URL or id. See \link{\code{googlesheets4::read_sheet}}.
#' @param creds Path to JSON creds file (service account token). 
#' @param cols List of columns that map to required parameters for \link{\code{new_project}}.
#' Defaults are provided.
.get_gs_project_tracking  <- function(sheet,
                                      creds = NULL,
                                      cols = c(name = "studyName", 
                                               pi = "studyPI",
                                               lead = "studyLeads",
                                               admin_user = NULL,
                                               abstract = "abstract", 
                                               institution = "institutions",
                                               funder = "fundingAgency", 
                                               initiative = "initiative")) {
  
  if(!is.null(creds) && file.exists(creds)) googlesheets4::gs4_auth(path = creds)
  # read_sheet will check if some auth is available
  projects <- googlesheets4::read_sheet(sheet) %>% 
    dplyr::select(all_of(cols)) 
  projects
}

#' @rdname get_new_project_data
#' Right now `get_new_project_data` is user-facing alias for `.get_googlesheets_new_project_data`
#' so we don't have to think too much about the underlying API being used; 
#' if data is eventually stored/retrieved with a different backend
#' (i.e. SQL database, Smartsheets, Airtable, etc.), this should point to the new method.

get_project_tracking <- .get_gs_project_tracking
 