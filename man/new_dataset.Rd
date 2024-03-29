% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\name{new_dataset}
\alias{new_dataset}
\title{Create new dataset with given items}
\usage{
new_dataset(name, parent, items, item_version = NULL, dry_run = TRUE)
}
\arguments{
\item{name}{Name of the dataset. It should be unique within the \code{parent} project.}

\item{parent}{Synapse id of parent project where the dataset will live.}

\item{items}{Id(s) of items to include.
Usually the same parent project storing the files, but in some cases it may be a different project.}

\item{item_version}{Integer for version that will be used for all items, e.g. 1.
Otherwise, "latest" or "stable_latest". See details.}

\item{dry_run}{If TRUE, don't actually store dataset, just return the data object for inspection or further modification.}
}
\description{
Create new dataset with given items
}
