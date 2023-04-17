% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login.R
\name{syn_login}
\alias{syn_login}
\title{Logs into Synapse.}
\usage{
syn_login(authtoken = Sys.getenv("SYNAPSE_AUTH_TOKEN"))
}
\arguments{
\item{authtoken}{Uses \code{SYNAPSE_AUTH_TOKEN} environmental variable, or a personal access token (PAT) can be provided.}
}
\description{
Wrapper around https://python-docs.synapse.org/build/html/index.html#synapseclient.Synapse.login
Username and password authentication is not supported.
Recommendation is to store \code{SYNAPSE_AUTH_TOKEN} in environment, so login can be used simply as \code{syn_login()}.
}
\examples{
\dontrun{
library(nfportalutils)
syn_login()
}
}