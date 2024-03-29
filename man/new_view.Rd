% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/views.R
\name{new_view}
\alias{new_view}
\title{Create a view}
\usage{
new_view(scope, project, name = "New View", include = "FILE")
}
\arguments{
\item{scope}{Character id(s) of project or folder container(s) in scope.}

\item{project}{Parent project id to create the view in.}

\item{name}{Name of view.}

\item{include}{Which entity type(s) to include in scope. Defaults to files.}
}
\description{
This creates a generic view, including by default just file entities and the default columns
(i.e. defaults to a generic fileview).
This is often useful to get ids of files for a large number of nested files by creating a temp fileview
(the alternative is to use \code{walk}, but if the tree structure is not regular it can be messy to parse the output).
}
