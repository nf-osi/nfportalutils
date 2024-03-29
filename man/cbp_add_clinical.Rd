% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbioportal.R
\name{cbp_add_clinical}
\alias{cbp_add_clinical}
\title{Export and add clinical data to cBioPortal dataset}
\usage{
cbp_add_clinical(clinical_data, ref_map, verbose = TRUE)
}
\arguments{
\item{clinical_data}{Clinical table query.}

\item{ref_map}{YAML file specifying the mapping of (NF) clinical metadata to cBioPortal model. See details.}

\item{verbose}{Whether to provide informative messages throughout.}
}
\description{
This should be run in an existing dataset package root.
}
\details{
Clinical data are mapped and exported according to a reference mapping.
Also reformatting of \code{PATIENT_ID}, \code{SAMPLE_ID} to contain only letters, numbers, points, underscores, hyphens;
in Nextflow processing any spaces gets replaced with underscores so that's the default here.
Does \emph{not} check for missing samples, as final validation via cBioPortal tool is still expected for that.
}
