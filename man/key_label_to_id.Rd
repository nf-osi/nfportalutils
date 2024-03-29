% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{key_label_to_id}
\alias{key_label_to_id}
\title{Query for schema key id given label}
\usage{
key_label_to_id(
  label,
  prefixed = TRUE,
  schema =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld"
)
}
\arguments{
\item{label}{The term label, a.k.a display name.}

\item{prefixed}{Boolean to indicate whether to include namespace prefix or return bare ID. Defaults to \code{TRUE}.}

\item{schema}{URL or local path to a .jsonld file which the schema is to be read from.}
}
\value{
The id if found, such as "bts:MyID", otherwise an empty character vector.
}
\description{
Utility to translate label to id using a schematic-generated schema.
}
