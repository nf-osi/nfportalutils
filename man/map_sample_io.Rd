% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nextflow_annotation_utils.R
\name{map_sample_io}
\alias{map_sample_io}
\title{Map sample input-output}
\usage{
map_sample_io(
  workflow = c("nf-rnaseq", "nf-sarek"),
  samplesheet,
  syn_out,
  sample_level = 2
)
}
\arguments{
\item{workflow}{Workflow.}

\item{samplesheet}{A local file or syn id of samplesheet.}

\item{syn_out}{Syn id of syn output destination with files of interest.}

\item{sample_level}{If caller is organized by sample, use 2 (default), if samples organized by caller, use 3. See details.}
}
\value{
A table with \code{sample} \code{level} \code{output_id} \code{output_name} \code{input_id}.
}
\description{
Wrapper to map sample inputs and outputs depending on workflow type.
}
