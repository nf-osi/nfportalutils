#' Create a new project
#'
#' Set up a new NF project with wiki, folders, fileview, and permissions.
#' Most parameters come from a project intake & data sharing plan (DSP) form.
#' Aside from default folders, folders are tailored for data mentioned in DSP.
#' The NF-OSI team is hard-coded to be admin in addition to the funder team indicated by `funder`.
#' Since this is intended for actual new projects, it will fail if a same existing project is detected.
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
#' @param datasets (Optional) Datasets for which folders will be created under main data folder ("Raw Data").
#' @param webview Whether to open web browser to view newly created project. Defaults to FALSE.
#' @param ... Additional arguments. Not used.
#' @return The project object.
#' @export
new_project <- function(name,
                        pi,
                        lead,
                        admin_user = NULL,
                        abstract,
                        institution,
                        funder = c("CTF", "GFF", "NTAP"),
                        initiative,
                        datasets = NULL,
                        webview = FALSE,
                        ...) {

  .check_login()

  project <- new_project_strict(name)

  # WIKI -----------------------------------------------------------------------#

  wiki <- add_default_wiki(project,
                           name,
                           pi,
                           lead,
                           funder,
                           initiative,
                           abstract,
                           institution)

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
  folders <- add_default_folders(project)
  data_folder <- folders[["Raw Data"]]

  # Create data-specific folders in "Raw Data"
  if(length(datasets)) make_folder(parent = data_folder$properties$id, folders = datasets)

  # Add Project Files and Metadata fileview, add NF schema; currently doesn't add facets
  fv <- add_default_fileview(project)

  if(webview) .syn$onweb(project)

  attr(project, "fileview") <- fv$properties$id
  return(project)
}

#' Create a strictly new project
#'
#' Internal handler for creating a project that
#' first checks whether project already exists and disallows overwriting.
#' For a less strict version that allows overwriting with a warning,
#' e.g. named `update_project`, implement with
#' `createOrUpdate = TRUE` and then compare createdOn and modifiedOn to issue a warning
#' (which would be more informative than current Python client).
#' @param project_name Name of project to be created.
#' @export
new_project_strict <- function(project_name) {
  id <- try(.syn$findEntityId(project_name))
  if(class(id) == "try-error") {
    # Error with 403 code if exists without writable permissions
    stop("Project not created because of name collision with a non-NF project.", call. = FALSE)
  } else if(class(id) == "character") {
    # Likely already administering project if returns actual synID
    stop("Project not created because of name collision with a current project. Check project entity?" , call. = FALSE)
  } else { # NULL
    project <- synapseclient$Project(project_name)
    project <- .syn$store(project, createOrUpdate = FALSE)
    project
  }
}

#' Create default project fileview
#'
#' @param project A project entity.
#' @export
add_default_fileview <- function(project) {
  view <- synapseclient$EntityViewSchema(
    name = "Project Files and Metadata",
    columns=list(
      synapseclient$Column(name="resourceType", columnType="STRING", maximumSize="50"),
      synapseclient$Column(name="assay", columnType="STRING", maximumSize="57"),
      synapseclient$Column(name="dataType", columnType="STRING", maximumSize="30"),
      synapseclient$Column(name="dataSubtype", columnType="STRING", maximumSize="13"),
      synapseclient$Column(name="fileFormat", columnType="STRING", maximumSize="13"),
      synapseclient$Column(name="diagnosis", columnType="STRING", maximumSize="39"),
      synapseclient$Column(name="tumorType", columnType="STRING", maximumSize="90"),
      synapseclient$Column(name="individualID", columnType="STRING", maximumSize="213"),
      synapseclient$Column(name="specimenID", columnType="STRING", maximumSize="300"),
      synapseclient$Column(name="nf1Genotype", columnType="STRING", maximumSize="8"),
      synapseclient$Column(name="nf2Genotype", columnType="STRING", maximumSize="7"),
      synapseclient$Column(name="species", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="modelSystemName", columnType="STRING", maximumSize="42"),
      synapseclient$Column(name="cellType", columnType="STRING", maximumSize="300"),
      synapseclient$Column(name="sex", columnType="STRING", maximumSize="50"),
      synapseclient$Column(name="age", columnType="STRING", maximumSize="50"),
      synapseclient$Column(name="experimentalCondition", columnType="STRING", maximumSize="58"),
      synapseclient$Column(name="progressReportNumber", columnType="INTEGER")
    ),
    parent = project,
    scopes = project,
    includeEntityTypes = list(synapseclient$EntityViewType$FILE),
    add_default_columns = TRUE)
  view <- .syn$store(view)
  invisible(view)
}


#' Make a user or group full admin of a Synapse entity
#'
#' Convenience method to set admin permissions
#' @param entity The Synapse entity, e.g. project or folder.
#' @param principal_id User or team id that will have the configured access to the entity.
#' @export
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
#' @export
make_folder <- function(parent, folders) {

  refs <- list()
  for (i in folders) {
    folder <- synapseclient$Folder(i, parent = parent)
    folder <- .syn$store(folder)
    refs[[i]] <- folder
  }
  return(refs)
}

#' Add default wiki
#'
#' Add the default wiki at project at creation or
#' use to retrofit projects where creators have not created a wiki.
#' @inheritParams new_project
#' @param project Synapse id of project.
#' @export
add_default_wiki <- function(project,
                             name,
                             pi,
                             lead,
                             funder,
                             initiative,
                             abstract,
                             institution) {

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

  return(wiki)

}

#' Create default folders
#'
#' A convenience wrapper around `make_folder` with NF defaults.
#' @param project The project Synapse id or object.
#' @param folders Names of the standard set of folders.
#' @export
add_default_folders <- function(project, folders = c("Analysis", "Milestone Reports", "Raw Data")) {
   make_folder(parent = project, folders)
}

#' Get and parse data from Google Sheets for initializing a new project
#'
#' Currently, project tracking data is stored in a private GoogleSheet.
#' For \code{\link{new_project}}, this wraps `googlesheets4` to get the needed data.
#'
#' If `creds` is not provided, i.e. there is no service account token,
#' then usage requires authenticating the Tidyverse API in the browser to get an API token on your behalf.
#' This doesn't actually give the googlesheets4 project access to any data
#' ("The Tidyverse API Packages project never receives your data or the permission to access your data."
#' -- see https://www.tidyverse.org/google_privacy_policy/.)
#' @name get_gs_project_tracking
#' @param sheet Sheet URL or id. See \code{\link[googlesheets4]{read_sheet}}.
#' @param creds Path to JSON creds file (service account token).
#' @param cols List of columns that map to required parameters for \code{\link{new_project}}.
#' Defaults are provided.
#' @export
get_gs_project_tracking  <- function(sheet,
                                      creds = NULL,
                                      cols = c(name = "studyName",
                                               pi = "studyPI",
                                               lead = "studyLeads",
                                               admin_user = NULL,
                                               abstract = "abstract",
                                               institution = "institutions",
                                               funder = "fundingAgency",
                                               initiative = "initiative",
                                               disease = "diseaseFocus",
                                               manifestations = "diseaseManifestations",
                                               grant_doi = "grantDOI"
                                               )
                                             ) {

  if(!is.null(creds) && file.exists(creds)) googlesheets4::gs4_auth(path = creds)
  # read_sheet will check if some auth is available
  projects <- googlesheets4::read_sheet(sheet) %>%
    dplyr::select(all_of(cols))
  projects
}

#' @rdname get_gs_project_tracking
#' @export
get_project_tracking <- get_gs_project_tracking
