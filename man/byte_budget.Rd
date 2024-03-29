% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_fix.R
\name{byte_budget}
\alias{byte_budget}
\title{Calculate byte budget for a schema}
\usage{
byte_budget(table, schema_cols = NULL, result = "remaining")
}
\arguments{
\item{table}{Existing Synapse table id or the table schema object used to retrieve the column types.}

\item{schema_cols}{Optional, this also can take a list of column characteristics; use when building from scratch and columns are not yet stored.
If given, \code{table} will be ignored.}

\item{result}{Return the summary number for "remaining" or "allocated", or return a TRUE/FALSE for "within" budget.}
}
\description{
Tables have a hard width limit of 64KB.
Given a current table schema, this does math for how many bytes remain or are already allocated.
Useful as an austerity measure if indeed one has a very large table,
or in other cases when philosophically being more principled in schema configuration.
}
\details{
See also:
\itemize{
\item https://rest-docs.synapse.org/rest/org/sagebionetworks/repo/model/table/ColumnType.html
}
}
