% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_utils.R
\name{check_access}
\alias{check_access}
\title{Check access}
\usage{
check_access(
  id,
  principal_id,
  access_type = c("CREATE", "UPDATE", "CHANGE_SETTINGS", "DOWNLOAD", "MODERATE", "READ",
    "CHANGE_PERMISSIONS", "DELETE")
)
}
\arguments{
\item{id}{The benefactor entity.}

\item{principal_id}{Group(s) for which to check the access type.}

\item{access_type}{Which access type(s) to check for; result summarizes whether there are permissions for \emph{all} types specified.}
}
\description{
Check access
}
