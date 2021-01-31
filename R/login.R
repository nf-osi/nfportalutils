#' Logs into Synapse.
#'
#' @param username a Synapse username (optional, not required if .synapseConfig available)
#' @param password a Synapse password (optional, not required if .synapseConfig available)
#' @examples
#' library(nfportalutils)
#' syn_login()
#' @import reticulate
#' @export
syn_login <- function(username = NULL, password = NULL){
  .syn <<- synapseclient$Synapse()
  .syn$login(username, password)
}

#' Checks .syn object exists.
#' @returns A message.
.check_login <- function(){
  if(!exists(".syn")){
    stop('Please run `nfportalutils::syn_login()` prior to running functions that require a connection to Synapse. (Alternatively, the Python `synapseclient` is unavailable.)')
  }
}
