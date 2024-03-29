% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register_study.R
\name{strlist_JSON}
\alias{strlist_JSON}
\title{Convert delimited record to JSON representation needed by a stringlist col schema}
\usage{
strlist_JSON(record, sep = ",", trim_ws = T)
}
\arguments{
\item{record}{Character vector of length one representing a single record.}

\item{sep}{Default = ",". The delimiter in the character string.}

\item{trim_ws}{Default = TRUE. Remove white space at the beginning and end of list items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST result).}
}
\description{
Internal helper that reuses and extends the utility of \code{.delim_string_to_vector}.
}
\keyword{internal}
