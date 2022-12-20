# Utils for querying and setting access

# -- QUERY ACCESS --------------------------------------------------------------#

#' Summarize file access for files within some view
#'
#' Some portion adapted from `@allaway`'s original script.
#' Common usages are:
#' - Check numbers of files are viewable and downloadable for registered Synapse users:
#' `summarize_file_access(principal_id = 273948, access_type = "DOWNLOAD", "syn16858331")`
#' - Check that files within the portal purview are actually editable for the NF-OSI Sage Team,
#'  because we need at least edit permissions for updating annotations (historically, some issues with hackathon projects, etc.):
#'  `summarize_file_access(principal_id = 3378999, access_type = "UPDATE", "syn16858331")`
#'
#' More complex usage:
#' - Check access for _multiple_ teams.
#'
#' This summarizes file access by using the smaller set of benefactors,
#' and returns a `data.table` with columns `benefactorId`, `principalId`, `access`, and `N`, which describes whether
#' `principalId` has _all_ the specified `access` to `benefactorId` and its `N` files.
#' This data can be used for further aggregation as needed.
#'
#' @inheritParams check_access
#' @param fileview_id Syn id of the view. View must include `benefactorId` and `type`.
#' @import data.table
#' @export
summarize_file_access <- function(principal_id, # 3378999 for NF-OSI
                                  access_type,
                                  fileview_id # "syn16858331"
                        ) {

  .check_login()
  tryCatch({
    view <- .syn$tableQuery(glue::glue("SELECT id,type,benefactorId FROM {fileview_id}"))
  }, error = function(e) stop("Could not query view!"))
  view <- as.data.table(view$asDataFrame())
  files_by_benefactor <- view[type == "file", .N, by = .(benefactorId)]
  access <- view[, check_access(benefactorId, principal_id, access_type), by = .(benefactorId)]
  # files_by_benefactor can be smaller than access because there are folders without files
  access <- merge(access, files_by_benefactor, all.x = TRUE)
  access[is.na(N), N := 0]
  return(access)
}

#' Check access
#'
#' @param id The benefactor entity.
#' @param principal_id Group(s) for which to check the access type.
#' @param access_type Which access type(s) to check for; result summarizes whether there are permissions for _all_ types specified.
#' @export
check_access <- function(id,
                         principal_id,
                         access_type = c("CREATE", "UPDATE", "CHANGE_SETTINGS", "DOWNLOAD", "MODERATE", "READ", "CHANGE_PERMISSIONS", "DELETE")) {

  access_type <- match.arg(access_type, several.ok = TRUE)
  stopifnot(is.numeric(principal_id))

  acl_result <- tryCatch({
    .syn$restGET(glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{id}/acl"))$resourceAccess %>%
    rbindlist(.)
  }, error = function(e) stop(glue::glue("Error for {id}: {e$message}")))

  setkey(acl_result, "principalId")
  # returns NA in accessType if principal does not have that access
  access_result <- acl_result[.(principal_id)][, .(access = all(access_type %in% accessType)), by = .(principalId)]
  return(access_result)
}


# -- SETTING ACCESS -------------------------------------------------------------#

#' Make public
#'
#' Sets READ/DOWNLOAD permissions for web and registered users equivalently to the "Make Public" button in Synapse UI.
#' TODO: For regular users this can be a one-and-done action, but for the DCC admin this likely entails some other actions,
#' such as updating a project tracking table, so a wrapper or "callback" functionality might be needed.
#'
#' @param id Synapse entity id.
#' @export
make_public <- function(id) {
  .check_login()
  ALL_REGISTERED_SYNAPSE_USERS_GROUP <- "273948"
  PUBLIC_GROUP <- "273949"
  # set registered synapse users to view, download
  .syn$setPermissions(entity = id,
                      principalId = ALL_REGISTERED_SYNAPSE_USERS_GROUP,
                      accessType = list("READ","DOWNLOAD"))

  # set public to view
  .syn$setPermissions(entity = id,
                      principalId = PUBLIC_GROUP,
                      accessType = list("READ"))
}


#' Provide access to a specific set of files using a query result.
#'
#' Sets READ/DOWNLOAD permissions for a specific user or team, provided a vector of entity IDs. Generally, we
#' do not set permissions this way, as it can create many, many ACLs/"local sharing settings" which will need to be removed at the
#' time of data publication. However, at the time of writing, one project (JHU Biobank) shares embargoed data but is
#' required to share only specific subsets of the files as needed by the data requestor (e.g. only MPNST tumor data, or only RNA-seq data).
#'
#' @param principal_id Synapse team or user id.
#' @param entity_ids Vector of entity ids.
#' @param create_dataset Optionally, create a dataset of the entity_ids, so that the user can easily retrieve them.
#' @param project_id If create_dataset=T, which project to create it in.
#' @param dataset_name Optional name for dataset to be created
#' @export
grant_specific_file_access <- function(principal_id, entity_ids, create_dataset = F, project_id = NULL, dataset_name = NULL) {
  # .check_login()

  if(create_dataset & is.null(project_id)){
    stop("project_id must be provided if create_dataset = T")
  }

  # set registered synapse users to view, download
  sapply(entity_ids, function(id){
    .syn$setPermissions(entity = id,
                      principalId = principal_id,
                      accessType = list("READ","DOWNLOAD"))
   })

  ##need to grab the current versions for dataset creation
  dataset_items <- lapply(entity_ids, function(id){
    vsn <- .syn$get(id, downloadFile = F)$versionNumber
    list(entityId = id, versionNumber = vsn)
  })

  if(is.null(dataset_name)){
    dataset_name <- glue::glue("Dataset {Sys.Date()} for {principal_id}")
  }

  if(create_dataset){
    dataset <- .syn$store(
      synapseclient$Dataset(
      name=dataset_name,
      parent=project_id,
      dataset_items=dataset_items)
      )

    # Add/remove specific Synapse IDs to/from the Dataset

    .syn$setPermissions(entity = dataset$properties$id,
                      principalId = principal_id,
                      accessType = list("READ","DOWNLOAD"))


    message(glue::glue('{emoji::emoji("thumbsup")} Dataset created at {dataset$properties$id}'))
  }

  message(glue::glue('{emoji::emoji("astonished")} Principal {principal_id} added to {length(entity_ids)}'))

  #TODO: set schema programmatically? might be easier to add annotations to schema in web client as needed to support principal_id...

}

