% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_to_stringlist.R
\name{.delim_string_to_vector}
\alias{.delim_string_to_vector}
\title{Convert a delimited string to vector, utility function.}
\usage{
.delim_string_to_vector(string, sep, trim_ws = T)
}
\arguments{
\item{string}{A character string.}

\item{sep}{Default = ",". The delimiter in the character string.}

\item{trim_ws}{Default = TRUE. Remove white space at the beginning and end of list items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST result).}
}
\description{
Converts a delimited string to a stringlist annotation and adjust the associated schema in the portal fileview.
}
