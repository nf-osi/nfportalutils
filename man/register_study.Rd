% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register_study.R
\name{register_study}
\alias{register_study}
\title{Register a NEW project for the NF Data Portal in \strong{Portal - Project View}}
\usage{
register_study(
  id,
  study_meta,
  summary,
  study_summary_table,
  portal_project_view = "syn52677631"
)
}
\arguments{
\item{id}{Synapse id of study.}

\item{study_meta}{List of annotations representing study meta.}

\item{summary}{Large summary string.}

\item{study_summary_table}{Id of where to store summary (can be any table with a \code{summary} LARGETEXT column).}

\item{portal_project_view}{View of DCC-managed projects (studies).}
}
\description{
Add relevant study metadata to the project as annotations.
Add to scope of NF-OSI data portal and management in \strong{Portal - Project View}.
}
