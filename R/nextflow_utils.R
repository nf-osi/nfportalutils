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
#' See the related \code{\link{map_sample_input_ss}} for mapping sample to inputs instead of outputs.
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
#' @inheritParams map_sample_output_rna_seq
#' @param workflow Workflow. 
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


#' Make annotations from workflow tool stats
#' 
#' Extracts a subset of [samtools stats](http://www.htslib.org/doc/samtools-stats.html),
#' and [picard stats](https://broadinstitute.github.io/picard/picard-metric-definitions.html) 
#' from workflow metafiles to surface as annotations. 
#' Files are expected depending on sequencing type; 
#' picard stats of interest are only for WGS/WES/targeted sequencing. 
#' Regarding the selection of stats, see the Genomic Data Commons (GDC) model for
#' [Aligned Reads](https://docs.gdc.cancer.gov/Data_Dictionary/viewer/#?view=table-definition-view&id=aligned_reads)  
#' 
#' @param samtools_stats_file Path to file/syn id of file with samtools stats produced by the workflow. 
#' @param picard_stats_file Path to file/syn id of file with picard stats produced by the workflow.
#' @export
tool_stats_to_annotations <- function(samtools_stats_file = NULL, 
                                      picard_stats_file = NULL) {
  if(is.null(samtools_stats_file)) {
    message("Samtools stats not available, skipping these annotations...")
    sam_stats <- NULL
  } else {
    sam_stats <- dt_read(samtools_stats_file)
    sam_stats <- sam_stats[, .(sample = Sample,
                               averageInsertSize = insert_size_average,
                               averageReadLength = average_length,
                               averageBaseQuality = average_quality,
                               pairsOnDifferentChromosomes = pairs_on_different_chromosomes,
                               readsDuplicatedPercent = reads_duplicated_percent,
                               readsMappedPercent = reads_mapped_percent,
                               totalReads = raw_total_sequences)]
  }
  
  if(is.null(picard_stats_file)) {
    message("Picard stats not available, skipping these annotations...")
    picard_stats <- NULL
  } else {
    # TO DO
    pic_stats <- dt_read(picard_stats_file)
    pic_stats <- pic_stats[, .(sample = Sample,
                               meanCoverage = MEAN_COVERAGE,
                               proportionCoverage10x = PCT_10X,
                               proportionCoverage30x = PCT_30X)]
  }
  result <- Reduce(function(x, y) merge(x, y, by = "sample"), 
                   Filter(is.data.table, list(sam_stats, picard_stats)))
  return(result)
}

#' Create annotations for processed aligned reads
#' 
#' Help put together annotation components for nextflow star-salmon outputs. 
#' Annotations come from several sources:
#' 1. Inherit some annotations on the original input files.
#' Requires a reference mapping of input files to use. 
#' Most property vals can be inherited by the derived files, e.g. assay type and sample info, 
#' but props like "comments" and "entityId" should NOT be inherited. 
#' Ideally, the data model itself should include inheritance rules to apply;
#' since that isn't possible currently, these exclusions are hard-coded as 
#' "non-inheritance heuristics" and overall functionality is problematic with other data models 
#' (which, for example, may have something called "notes" instead of "comments"). 
#' See \code{\link{inherit_input_annotations}}.
#' 
#' 2. Extract metrics from workflow auxiliary files to surface as annotations. 
#' See helper \code{\link{tool_stats_to_annotations}}.
#' 
#' 3. Manually add annotations that can't (yet?) be derived from #1 or #2.  
#' Has to be done outside of this util.
#' 
#' A partial manifest will always be returned; the param `update` specifies
#' whether annotations should be applied.
#' 
#' @inheritParams get_by_prop_from_json_schema
#' @inheritParams tool_stats_to_annotations
#' @param template URI of data template in model, prefixed if needed.
#' @param sample_io Table mapping input to outputs. 
#' @param update Whether to apply annotations. See details.
#' @param verbose Give verbose reports for what's happening.
#' @export
annotate_aligned_reads <- function(template,
                                   schema,
                                   sample_io,
                                   samtools_stats_file = NULL,
                                   picard_stats_file = NULL,
                                   update = FALSE, 
                                   verbose = TRUE) {
  
  props <- get_dependency_from_json_schema(id = template, schema = schema)
  # Hard-coding props to NEVER inherit in the template
  select <- props[!props %in% c("comments", "entityId", "fileFormat", "dataType", "dataSubtype")]
  anno_set_1 <- inherit_input_annotations(sample_io, select = select)
  if(verbose) message("Inherited some annotations from inputs.")
  anno_set_2 <- tool_stats_to_annotations(samtools_stats_file, picard_stats_file)
  if(verbose && length(anno_set_2)) message("Extracted stats from workflow metafiles for some annotations.")
  if(verbose) message("Merging annotation components...")
  
  annotations <- Reduce(function(x, y) merge(x, y, by = "sample", allow.cartesian = TRUE), 
                        Filter(is.data.table, list(anno_set_1, 
                                                   anno_set_2, 
                                                   sample_io[, .(sample, entityId = output_id, Filename = output_name)])))
  annotations[, dataType := "AlignedReads"]
  annotations[, dataSubtype := "processed"]
  return(annotations)
}


#' Inherit annotations
#' 
#' Have output/derived entities inherit annotations from input entities.
#' If there are multiple inputs, inherit properties from the FIRST input.
#' Other options may be implemented in the future.
#' 
#' @inheritParams copy_annotations
#' @param sample_io A `data.table` that minimally contains `input_id` and `output_id`.
#' @param update Whether to have annotations applied immediately or return manifest component only.
#' @import data.table
inherit_input_annotations <- function(sample_io, select = NULL, update = FALSE) {
  from_to <- sample_io[, .(from = sapply(input_id, first), to = output_id)]
  annotations <- list()
  for(i in 1:nrow(from_to)) {
    annotations[[i]] <- copy_annotations(from_to[i, from], from_to[i, to], select, update)
  }
  if(!update) {
    names(annotations) <- sample_io$sample
    annotations <- lapply(annotations, reticulate::py_to_r) %>% rbindlist(fill = T, idcol = "sample")
  }
  return(annotations)
}
