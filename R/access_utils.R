# Utils for querying and setting access

# -- QUERY ACCESS --------------------------------------------------------------#

#' Summarize file access for files within some view
#' 
#' Common usages are:
#' - Check numbers of files are viewable and downloadable for registered Synapse users:
#' `summarize_file_access("DOWNLOAD", principal_id = 273948, "syn16858331")`
#' - Check that files within the portal purview are actually editable for the NF-OSI Sage Team,
#'  because we need at least edit permissions for updating annotations (historically, some issues with hackathon projects, etc.):
#'  `summarize_file_access("DOWNLOAD", principal_id = 3378999, "syn16858331")`
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


