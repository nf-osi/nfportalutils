#' Create default project fileview
#'
#' @param project A project entity.
#' @export
add_default_fileview <- function(project) {
  view <- synapseclient$EntityViewSchema(
    name = "Project Files and Metadata",
    columns=list(
      synapseclient$Column(name="contentType", columnType="STRING", maximumSize="20"),
      synapseclient$Column(name="resourceType", columnType="STRING", maximumSize="50"),
      synapseclient$Column(name="assay", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="dataType", columnType="STRING", maximumSize="30"),
      synapseclient$Column(name="dataSubtype", columnType="STRING", maximumSize="13"),
      synapseclient$Column(name="fileFormat", columnType="STRING", maximumSize="13"),
      synapseclient$Column(name="diagnosis", columnType="STRING", maximumSize="39"),
      synapseclient$Column(name="tumorType", columnType="STRING", maximumSize="90"),
      synapseclient$Column(name="individualID", columnType="STRING_LIST", maximumSize="50", maximumListLength = "50"),
      synapseclient$Column(name="specimenID", columnType="STRING_LIST", maximumSize="50", maximumListLength = "50"),
      synapseclient$Column(name="nf1Genotype", columnType="STRING", maximumSize="8"),
      synapseclient$Column(name="nf2Genotype", columnType="STRING", maximumSize="20"),
      synapseclient$Column(name="species", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="modelSystemName", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="cellType", columnType="STRING_LIST", maximumSize="50", maximumListLength = "50"),
      synapseclient$Column(name="sex", columnType="STRING", maximumSize="30"),
      synapseclient$Column(name="age", columnType="STRING", maximumSize="50"),
      synapseclient$Column(name="experimentalCondition", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="progressReportNumber", columnType="INTEGER")
    ),
    parent = project,
    scopes = project,
    includeEntityTypes = list(synapseclient$EntityViewType$FILE, synapseclient$EntityViewType$FOLDER),
    add_default_columns = TRUE,
    addAnnotationColumns = FALSE)
  view <- .syn$store(view)
  invisible(view)
}


#' Make a user or group full admin of a Synapse entity
#'
#' Convenience method to set admin permissions
#' @param entity The Synapse entity, e.g. project or folder.
#' @param principal_id User/team name or id (e.g. "NF-OSI Sage Team", "3378999", "nf-bot", or "3423450") that will have the configured access to the entity.
#' @export
make_admin <- function(entity, principal_id) {
  if(is_valid_user(principal_id) || is_valid_team(principal_id)) {
    admin <- .syn$setPermissions(entity = entity,
                                 principalId = principal_id,
                                 accessType = list('DELETE', 'CHANGE_SETTINGS', 'MODERATE', 'CREATE', 'READ','DOWNLOAD', 'UPDATE', 'CHANGE_PERMISSIONS'))
  } else {
    warning("Principal specified is not a valid user or team. Ignoring, please correct and resolve manually.")
    return(FALSE)
  }
}

#' Create project folders
#'
#' Use to set up a scaffold of standard upper-level folders as well as
#' customized data folders within "Raw Data" for a new project.
#' @param parent The Synapse id or object that should be the parent container, i.e. the project or another folder.
#' @param folders List giving one or more folder names of folder(s) to create.
#' @return A list of the created folder object(s).
#' @export
#' @examples
#' \dontrun{
#' datasets <- list("sequencing data", "imaging data")
#' assays <- c("rnaSeq", "immunohistochemistry")
#' for(i in seq_along(datasets)) attr(datasets[[i]], "assay") <- assays[[i]]
#' make_folder(parent = "syn26462036", datasets)
#'}
make_folder <- function(parent, folders) {

  refs <- list()
  for (i in folders) {
    # If datasets have "assay" or other attributes, this is added as annotations
    folder <- synapseclient$Folder(i, parent = parent, annotations = attributes(i))
    folder <- .syn$store(folder)
    refs[[i]] <- folder
  }
  return(refs)
}

#' Add default wiki
#'
#' Add the default wiki at project at creation or
#' use to retrofit projects where creators have not created a wiki.
#' @param project Synapse id of project.
#' @param name Name of the project/study.
#' @param pi Name of the principal investigator.
#' @param lead Name(s) of the project lead/data coordinator, comma-sep if multiple, e.g. "Jane Doe, John Doe".
#' @param funder The funding agency.
#' @param initiative Title of funding initiative, e.g. "Young Investigator Award".
#' @param abstract Project abstract/description.
#' @param institution Affiliated institution(s), **semicolon-sep if multiple**, e.g. "Stanford University; University of California, San Francisco".
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

#' Check that is valid user in Synapse
#' @keywords internal
is_valid_user <- function(id) {
  status <- tryCatch(
    .syn$getUserProfile(id), error = function(e) return(NULL)
    )
  if(length(status)) return(TRUE) else return(FALSE)
}

#' Check that is valid team in Synapse
#' @keywords internal
is_valid_team <- function(id) {
  status <- tryCatch(
    .syn$getTeam(id), error = function(e) return(NULL)
  )
  if(length(status)) return(TRUE) else return(FALSE)
}
