#' Parse nextflow samplesheet for sample inputs
#' 
#' Samplesheets are used in rnaseq pipelines,
#' and are defined here: https://nf-co.re/rnaseq/usage#full-samplesheet. 
#' After the pipeline is run, it should be found in an output folder called `pipeline_info`.
#' 
#' This is a simple helper to get a mapping of sample names to input files 
#' (usually one-to-many but can be one-to-one) to pass to downstream as a table.
#' 
#' @param samplesheet A local file or syn id of samplesheet.
#' @param parse_fun Function implementing how to parse samples in samplesheet.
#' @import data.table
#' @export
map_sample_input_ss <- function(samplesheet, 
                             parse_fun = function(x) gsub("_T[0-9]$", "", x)) {
  
  ss <- dt_read(samplesheet)
  ss[, input_syn_1 := bare_syn_id(fastq_1)] # Get synId from URI
  ss[, input_syn_2 := bare_syn_id(fastq_2)] 
  ss[, sample := parse_fun(sample)] # Parse sample from "sample" col
  
  # File inputs for each sample specimen
  sample_inputs <- ss[, .(input_id = list(c(input_syn_1, input_syn_2))), by = sample]
  return(sample_inputs)
}

#' Map sample to output RNA-seq files
#' 
#' In specified location where workflow has deposited the outputs (i.e. "result" directory),
#' map out the relevant processed files based on file extensions and tie these files to source samples. 
#' This relies on expected file locations and file naming convention of the specific workflow.
#' WARNING: Be aware that this may not work using a different version of workflow 
#' or if files are manually re-organized post-write to Synapse.   
#' With the known Nextflow workflow, outputs are organized by sample-id folders,
#' e.g. result/salmon/<sample>/<file>, so this looks for processed files within one level of nesting.
#' 
#' See the related \code{\link{map_sample_input}} for mapping sample to inputs instead of outputs.
#' 
#' @param syn_out Syn id of syn output destination (folder) with files of interest. 
#' @import data.table
#' @export
map_sample_output_rna_seq <- function(syn_out) {
  
  outputs <- local_view(syn_out)
  
  # maps intermediate level of processed (.bam/.bai files)
  processed <- outputs[grepl("*.bam$|*.bai$", name), .(name, id)]
  processed[, sample :=  gsub("[.].*", "", name)]
  processed[, level := 2L]
  
  # maps next level of processed (.sf files), which are nested
  folders <- outputs[type == "org.sagebionetworks.repo.model.Folder", .(name, id)]
  nested_outputs <- local_view(folders$id, idcol = "sample", idmap = setNames(folders$name, folders$id))
  nested_processed <- nested_outputs[grepl("*.sf$", name), .(name, id, sample)]
  nested_processed[, level := 3L]
  
  sample_outputs <- rbind(processed, nested_processed)
  data.table::setnames(sample_outputs, c("name", "id"), c("output_name", "output_id"))
  sample_outputs[, .(output_id = list(output_id), output_name = list(output_name)), by = .(sample, level)]
  return(sample_outputs)
}

#' Map sample input-output
#' 
#' Wrapper to map sample inputs and outputs depending on workflow type.
#' Mapping sample inputs use the samplesheet, but mapping outputs vary slightly.
#' 
#' @inheritParams map_sample_input_ss
#' @inheritParams map_sample_out_rna_seq
#' @return A table with `sample` `level` `output_id` `output_name` `input_id`.
#' @export
map_sample_io <- function(workflow = c("nf-rna-seq", "nf-exome-seq"),
                          samplesheet,
                          syn_out) {
  
  workflow <- match.arg(workflow)
  sample_inputs <- map_sample_input_ss(samplesheet)
  
  if(workflow == "nf-rna-seq") {
    sample_outputs <- map_sample_output_rna_seq(syn_out)
  } else if(workflow == "nf-exome-seq") {
    sample_outputs <- map_sample_output_exome_seq(syn_out)
  }
  
  # Check that sample ids in sample_outputs are in sample_inputs
  # Presumed OK for sample_inputs to contain samples *not* in outputs 
  # (e.g. maybe no outputs if doesn't pass QC checks) but not OK for vice versa
  stopifnot(all(unique(sample_outputs$sample) %in% unique(sample_inputs$sample)))
  sample_io <- merge(sample_outputs, sample_inputs, by = "sample", all.x = TRUE, all.y = FALSE)
  return(sample_io)
}

#' Extract synapse id from URI or other string
#' 
#' @param uri URI or string containing embedded Synapse id.
#' @keywords internal
bare_syn_id <- function(uri) regmatches(uri, regexpr("syn[0-9]{8}", uri))

