#' Logs into Synapse.
#' 
#' Wrapper around https://python-docs.synapse.org/build/html/index.html#synapseclient.Synapse.login
#' Similarly, providing no args will default to highest preference of using a `SYNAPSE_AUTH_TOKEN` env var to log in.
#' @param username a Synapse username (optional, not required if .synapseConfig available)
#' @param password a Synapse password (optional, not required if .synapseConfig available)
#' @param authtoken Uses `SYNAPSE_AUTH_TOKEN` environmental variable, or a personal access token string can be provided.
#' @examples
#' \dontrun{
#' library(nfportalutils)
#' syn_login()
#' }
#' @import reticulate
#' @export
syn_login <- function(username = NULL, password = NULL, authtoken = Sys.getenv("SYNAPSE_AUTH_TOKEN")){
  .syn <<- synapseclient$Synapse()
  if (authtoken == "") {
    .syn$login(username, password)
  } else {
    .syn$login(authToken = authtoken)
  }
  invisible(.syn)
}

#' Checks .syn object exists.
#' @returns A message.
#' @keywords internal
.check_login <- function(){
  if(!exists(".syn")){
    stop('Please run `nfportalutils::syn_login()` prior to running functions that require a connection to Synapse. (Alternatively, the Python `synapseclient` is unavailable.)')
  }else if(capture.output(.syn) == "<pointer: 0x0>"){
    stop('Please run `nfportalutils::syn_login()` prior to running functions that require a connection to Synapse. (Alternatively, the Python `synapseclient` is unavailable.)')
  }
}
