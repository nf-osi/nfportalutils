% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbioportal.R
\name{check_cbp_study_id}
\alias{check_cbp_study_id}
\title{Check that in valid cBioPortal study dataset root}
\usage{
check_cbp_study_id()
}
\value{
\code{cancer_study_id} for the current cBioPortal cancer study.
}
\description{
The \verb{cbp_add*} functions need to be run while in the study package root.
This checks in valid study directory and returns the \code{cancer_study_id}.
}
\keyword{internal}
