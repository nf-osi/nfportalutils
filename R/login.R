#' Logs into Synapse.
#'
#' Wrapper around https://python-docs.synapse.org/build/html/index.html#synapseclient.Synapse.login
#' Username and password authentication is not supported.
#' Recommendation is to store `SYNAPSE_AUTH_TOKEN` in environment, so login can be used simply as `syn_login()`.
#'
#' @param authtoken Uses `SYNAPSE_AUTH_TOKEN` environmental variable, or a personal access token (PAT) can be provided.
#' @examples
#' \dontrun{
#' library(nfportalutils)
#' syn_login()
#' }
#' @import reticulate
#' @export
syn_login <- function(authtoken = Sys.getenv("SYNAPSE_AUTH_TOKEN")){
  .syn <<- synapseclient$Synapse()
  .syn$login(authToken = authtoken)
  invisible(.syn)
}

#' Checks .syn object exists.
#' @returns A message.
#' @keywords internal
.check_login <- function(){
  if(!exists(".syn") & !exists("syn")){
    stop('Please run `nfportalutils::syn_login()` or `synapser::synLogin()` prior to running functions that require a connection to Synapse. (Alternatively, the Python `synapseclient` is unavailable.)')
  }
}
