#' Create new project
#' @description Use to set up a new project, including project metatada, default folders, wiki, fileview, and permissions; 
#' some of the important parameters should come from a project intake form.
#' This helps have more consistency for the wiki and fileview across projects. 
#' The structure of the fileview is also important for the NF Data Portal, 
#' which selects common columns from individual project fileviews. 
#' 
#' TO DO: Default folders are tailored to data types mentioned in data sharing form. 
#' 
#' @param project_name Project name provided by project lead in the intake form.
#' @param project_abstract Project abstract provided by project lead in the intake form,
#' which will be written into the wiki.
#' @param funder An NF funder for which admin permissions will be added, currently one of c("CTF", "GFF", "NTAP").
#' @param webview Whether to open web browser to view newly created project. Defaults to FALSE.

new_project <- function(project_name, project_abstract, funder = c("CTF", "GFF", "NTAP"), webview = FALSE) {
  
  .check_login()
  
  content <- glue::glue("
  # {project_name}
  ## Initiative and/or Funder Info
  ### Principal Investigator: Insert PI name here
  ### Project Lead / Data Coordinator: Insert Data Coordinator name here
  ### Institution: Insert Institution name here
  ### Project Description: 
  {project_abstract}
  ")
  
  # Add a subpage for links to NF Data Curator App as of December 2021
  
  project <- synapseclient$Project(project_name)
  project <- .syn$store(project)
  
  wiki <- synapse$Wiki(owner = project,
                       title = project_name,
                       markdown = content)
  
  wiki <- .syn$store(wiki)
  
  # name and create folders
  data_folder1 = synapse$Folder('Analysis', parent = project)
  data_folder1 = syn$store(data_folder1)
  data_folder2 = synapse$Folder('Milestone Reports', parent = project)
  data_folder2 = syn$store(data_folder2)
  data_folder3 = synapse$Folder('Raw Data', parent = project)
  data_folder3 = syn$store(data_folder3)
  
  
  # set NF-OSI Sage Team permissions; currently sets to full admin
  NF_sharing <- syn$setPermissions(entity=project, 
                                  principalId=3378999, 
                                  accessType=list('DELETE', 'CHANGE_SETTINGS', 'MODERATE', 'CREATE', 'READ','DOWNLOAD', 'UPDATE', 'CHANGE_PERMISSIONS'))
  
  # set grant funding partner team Admin permissions
  funder <- match.arg(funder)
  funder <- switch(funder,
                   CTF = 3359657, ##CTF team
                   GFF = 3406072, ##GFF Admin team
                   NTAP = 3331266) ##NTAP Admin team
  
  funder_sharing <- .syn$setPermissions(entity = project,
                                       principalId = funder,
                                       accessType=list('DELETE', 'CHANGE_SETTINGS', 'MODERATE', 'CREATE', 'READ','DOWNLOAD', 'UPDATE', 'CHANGE_PERMISSIONS'))
  
  # add Project Files and Metadata fileview, add NF schema; currently doesn't add facets
  view <- synapseclient$EntityViewSchema(name="Project Files and Metadata",
                                    columns=list(
                                     synapse$Column(name="assay", columnType="STRING", maximumSize="57"),
                                     synapse$Column(name="consortium", columnType="STRING", maximumSize="24"),
                                     synapse$Column(name="dataSubtype", columnType="STRING", maximumSize="13"),
                                     synapse$Column(name="dataType", columnType="STRING", maximumSize="30"),
                                     synapse$Column(name="diagnosis", columnType="STRING", maximumSize="39"),
                                     synapse$Column(name="tumorType", columnType="STRING", maximumSize="90"),
                                     synapse$Column(name="fileFormat", columnType="STRING", maximumSize="13"),
                                     synapse$Column(name="fundingAgency", columnType="STRING", maximumSize="12"),
                                     synapse$Column(name="individualID", columnType="STRING", maximumSize="213"),
                                     synapse$Column(name="nf1Genotype", columnType="STRING", maximumSize="8"),
                                     synapse$Column(name="nf2Genotype", columnType="STRING", maximumSize="7"),
                                     synapse$Column(name="species", columnType="STRING", maximumSize="15"),
                                     synapse$Column(name="resourceType", columnType="STRING", maximumSize="50"),
                                     synapse$Column(name="isCellLine", columnType="STRING", maximumSize="50"),
                                     synapse$Column(name="isMultiSpecimen", columnType="STRING", maximumSize="50"),
                                     synapse$Column(name="isMultiIndividual", columnType="STRING", maximumSize="50"),
                                     synapse$Column(name="studyId", columnType="ENTITYID"),
                                     synapse$Column(name="studyName", columnType="LARGETEXT"),
                                     synapse$Column(name="specimenID", columnType="STRING", maximumSize="300"),
                                     synapse$Column(name="sex", columnType="STRING", maximumSize="50"),
                                     synapse$Column(name="age", columnType="STRING", maximumSize="50"),
                                     synapse$Column(name="readPair", columnType="INTEGER"),
                                     synapse$Column(name="progressReportNumber", columnType="INTEGER"),
                                     synapse$Column(name="accessType", columnType="STRING", maximumSize="50"),
                                     synapse$Column(name="accessTeam", columnType="USERID"),
                                     synapse$Column(name="cellType", columnType="STRING", maximumSize="300"),
                                     synapse$Column(name="modelOf", columnType="STRING", maximumSize="50"),
                                     synapse$Column(name="compoundName", columnType="STRING", maximumSize="156"),
                                     synapse$Column(name="experimentalCondition", columnType="STRING", maximumSize="58"),
                                     synapse$Column(name="modelSystemName", columnType="STRING", maximumSize="42"),
                                     synapse$Column(name="isXenograft", columnType="STRING", maximumSize="5"),
                                     synapse$Column(name="transplantationType", columnType="STRING", maximumSize="50")),
                                   parent=project,
                                   scopes=project,
                                   includeEntityTypes=list(synapse$EntityViewType$FILE),
                                   add_default_columns=TRUE)
  view <- .syn$store(view)
  if(webview) .syn$onweb(project)
  #TODO: add snippet to add the project to the Portal Files view scope
  #TODO: add snippet to add the project to the Portal Studies table as a row
}
