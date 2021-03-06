% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_to_stringlist.R
\name{.replace_string_column_with_stringlist_column}
\alias{.replace_string_column_with_stringlist_column}
\title{Modify a single annotation on a single file.}
\usage{
.replace_string_column_with_stringlist_column(
  table_id,
  column_name,
  max_str_len
)
}
\arguments{
\item{synapse_id}{A synapse entity id.}

\item{key}{The key of the annotation to modify.}

\item{value}{The value to change the annotation to.}
}
\description{
Modifies a single annotation value on a single (existing) synapse file. Guts of this ripped from @jaeddy gist (https://gist.github.com/jaeddy/1cf49f7851945beedb39d431134734af)
}
