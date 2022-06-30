#' Parse nextflow samplesheet for sample inputs
#' 
#' Samplesheets are used in rnaseq pipelines, defined here: https://nf-co.re/rnaseq/usage#full-samplesheet. 
#' After the pipeline is run, it should be found in an output folder called `pipeline_info`.
#' This is a simple helper to get a mapping of sample ids to input files (either one-to-many or one-to-one) as a table.
#' 
#' @param samplesheet A local file or syn id of samplesheet.
#' @param parse_fun Function implementing how to parse samples in samplesheet.
#' @import data.table
#' @export
map_sample_input_ss <- function(samplesheet,
                                parse_fun = function(x) gsub("_T[0-9]$", "", x)) {
  
  ss <- dt_read(samplesheet)
  # Annoyingly, headers are not standard and can use fastq1 instead of fastq_1
  if("fastq1" %in% names(ss)) setnames(ss, c("fastq1", "fastq2"), c("fastq_1", "fastq_2"))
  
  ss[, input_syn_1 := bare_syn_id(fastq_1)] # Get synId from URI
  ss[, input_syn_2 := bare_syn_id(fastq_2)] 
  ss[, sample := parse_fun(sample)] # Parse sample from "sample" col
  
  # File inputs for each sample specimen
  sample_inputs <- ss[, .(input_id = list(na.omit(c(input_syn_1, input_syn_2)))), by = sample]
  return(sample_inputs)
}


#' Map sample to output from nf-rnaseq
#' 
#' See https://nf-co.re/rnaseq. Given location where workflow has deposited the outputs,
#' map relevant processed files based on extensions and link these files to source samples. 
#' So for `results/star_salmon/<sample>/<file>`, the URI to pass in is the `star_salmon` folder.
#' Warning: Reliance on certain file structure and naming convention can make this somewhat brittle! 
#' 
#' See the related \code{\link{map_sample_input_ss}} for mapping sample to inputs instead of outputs.
#' 
#' @param syn_out Syn id of syn output destination with files of interest. 
#' @import data.table
#' @export
map_sample_output_rnaseq <- function(syn_out) {
  
  outputs <- local_view(syn_out)
  
  # maps intermediate level of processed (.bam/.bai files)
  processed <- outputs[grepl("*.bam$|*.bai$", name), .(name, id)]
  processed[, sample :=  gsub("[.].*", "", name)]
  
  # maps next level of processed (.sf files), which are nested
  folders <- outputs[type == "org.sagebionetworks.repo.model.Folder", .(name, id)]
  nested_outputs <- local_view(folders$id, idcol = "sample", idmap = setNames(folders$name, folders$id))
  nested_processed <- nested_outputs[grepl("*.sf$", name), .(name, id, sample)]
  
  sample_outputs <- rbind(processed, nested_processed)
  sample_outputs[, workflow := "STAR and Salmon"]
  data.table::setnames(sample_outputs, c("name", "id"), c("output_name", "output_id"))
  return(sample_outputs)
}


#' Map sample to output from nf-sarek
#' 
#' See https://nf-co.re/sarek. Processed outputs are nested by sample and variant callers, i.e. 
#' `*VariantCalling/<TUMOR_vs_NORMAL>/<CALLER>`. This walks through the output destination (URI of `*VariantCalling`)
#' with similar intention to \code{\link{map_sample_output_rnaseq}}, but for Sarek outputs.
#' 
#' @param syn_out Syn id of syn output destination with files of interest. 
#' @import data.table
#' @return A `data.table` with cols `caller` `caller_path` `caller_syn` `output_name` 
#' `output_name` `output_id` `sample`
#' @export
map_sample_output_sarek <- function(syn_out) {
  
  # `walk` can be very slow
  ls <- walk(syn_out)
  
  outputs <- rbindlist(
    lapply(ls[3:length(ls)], function(out) { 
      if(!length(out[[3]])) return()
      as.data.table(
        c(setNames(lapply(out[[1]], function(x) rep(x, length(out[[3]]))), c("caller_path", "caller_syn")), 
          output_name = list(sapply(out[[3]], `[[`, 1)), 
          output_id = list(sapply(out[[3]], `[[`, 2))
        )
      )
    })
  )
  paths <- strsplit(outputs$caller_path, "/", fixed = TRUE)
  outputs[, sample := sapply(paths, `[[`, 2)]
  outputs[, sample := strsplit(sample, "_vs_")]
  outputs[, workflow := sapply(paths, `[[`, 3)]
  return(outputs)
}


#' Map sample input-output
#' 
#' Wrapper to map sample inputs and outputs depending on workflow type. 
#' 
#' @inheritParams map_sample_input_ss
#' @inheritParams map_sample_output_rnaseq
#' @param workflow Workflow. 
#' @return A table with `sample` `level` `output_id` `output_name` `input_id`.
#' @export
map_sample_io <- function(workflow = c("nf-rnaseq", "nf-sarek"),
                          samplesheet,
                          syn_out) {
  
  workflow <- match.arg(workflow)
  sample_inputs <- map_sample_input_ss(samplesheet)
  
  if(workflow == "nf-rnaseq") {
    sample_outputs <- map_sample_output_rnaseq(syn_out)
  } else if(workflow == "nf-sarek") {
    sample_outputs <- map_sample_output_sarek(syn_out)
    # sample can contain 2 samples (tumor vs normal from same indiv) -> take first
    sample_outputs[, sample := sapply(sample, first)]
  }
  
  # Check that sample ids in sample_outputs are in sample_inputs
  # Presumed OK for sample_inputs to contain samples *not* in outputs 
  # (maybe no outputs if doesn't pass QC checks) but not OK vice versa
  stopifnot(all(unique(sample_outputs$sample) %in% unique(sample_inputs$sample)))
  sample_io <- merge(sample_outputs, sample_inputs, by = "sample", all.x = TRUE, all.y = FALSE)
  return(sample_io)
}


#' Make annotations from workflow tool stats
#' 
#' Extracts a subset of [samtools stats](http://www.htslib.org/doc/samtools-stats.html),
#' and [picard stats](https://broadinstitute.github.io/picard/picard-metric-definitions.html) 
#' from workflow metafiles to surface as annotations. 
#' Note that picard stats of interest are only for WGS/WES/targeted sequencing. 
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
                               pairsOnDifferentChr = pairs_on_different_chromosomes,
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


#' Derive annotations for processed output data
#' 
#' Build annotations through inheritance from inputs. If multiple inputs, inherit props from the FIRST input. 
#' Files that pass through this naturally have `dataSubtype` set to "processed" and `fileFormat` set 
#' to the actual new file format. In the future, `template` itself may define `format` so we don't need to specify explicitly.
#' 
#' @param format File format of the processed data.
#' @keywords internal
derive_annotations <- function(sample_io,
                               template,
                               schema,
                               format,
                               verbose = TRUE) {
  
  pattern <- paste0("[.]", format, "(.gz)?$")
  x <- sample_io[grep(pattern, output_name)] 
  n <- nrow(x)
  if(!length(n)) stop(glue::glue("Expected {format} files not found. Are you annotating the right files?"))
  if(verbose) message("Creating annotations for ", n, " files")
  
  props <- get_dependency_from_json_schema(id = template, schema = schema)
  props <- props[!props %in% c("comments", "entityId", "fileFormat", "dataType", "dataSubtype")]
  
  from <- sapply(sample_io$input_id, `[`, 1)
  to <- sample_io$output_id
  annotations <- Map(function(f, t) copy_annotations(f, t, select = props), from, to) %>%
    reticulate::py_to_r() %>% rbindlist(fill = T, idcol = "sample")
  
  annotations <- merge(annotations, 
                       x[, .(sample, entityId = output_id, Filename = output_name, workflow)], 
                       by = "sample", allow.cartesian = TRUE)
  annotations[, dataSubtype := "processed"]
  annotations[, fileFormat := format]
  return(annotations)
} 


#' Annotate processed aligned reads
#' 
#' Put together annotation components for nextflow star-salmon outputs. Annotations come from several sources:
#' 1. Inherit some annotations on the original input files. Requires a reference mapping of input files to use. 
#' Most prop vals can be inherited by the derived files, e.g. assay type, but not for "comments" or "entityId". 
#' Ideally, the data model itself should include inheritance rules; since that isn't possible currently, 
#' we hard-code lots of stuff, so this is hard to generalize for other data models. 
#' 
#' 2. Extract metrics from auxiliary files to surface as annotations. See  \code{\link{tool_stats_to_annotations}}.
#' 
#' 3. Manually add annotations that can't (yet?) be derived from #1 or #2. Has to be done outside of this util.
#' 
#' Always returns a "partial" manifest; the param `update` specifies whether annotations should be applied.
#' 
#' @inheritParams get_by_prop_from_json_schema
#' @inheritParams tool_stats_to_annotations
#' @param template (Optional) URI of template in data model to use, prefixed if needed. 
#' Can specify different model/version, but in some cases may not work well.
#' @param sample_io Table mapping input to outputs, where outputs are expected to be .bam files only!
#' @param update Whether to apply annotations.
#' @param verbose Give verbose reports for what's happening.
#' @export
annotate_aligned_reads <- function(sample_io,
                                   samtools_stats_file = NULL,
                                   picard_stats_file = NULL,
                                   template = "bts:ProcessedAlignedReadsTemplate",
                                   schema = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                                   verbose = TRUE,
                                   update = FALSE) {
  
  anno_base <- derive_annotations(sample_io, template, schema, format = "bam", verbose)
  anno_qc <- tool_stats_to_annotations(samtools_stats_file, picard_stats_file)
  if(verbose && length(anno_qc)) message("Created annotations from workflow metrics.")
  
  annotations <- Reduce(function(x, y) merge(x, y, by = "sample"), 
                        Filter(is.data.table, list(anno_base, 
                                                   anno_qc)))
  annotations[, dataType := "AlignedReads"]
  annotations[, sample := NULL] # bc internal usage, not for actual annotation
  if(update) {
    annotate_with_manifest(annotations)
    if(verbose) message("Applied annotations.")
  }
  return(annotations)
}


#' Annotate processed expression output
#' 
#' @inheritParams annotate_aligned_reads
#' @param sample_io Table mapping input to outputs, where outputs are expected to be .sf files.
#' @export
annotate_expression <- function(sample_io,
                                template = "bts:ProcessedExpressionTemplate",
                                schema = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                                verbose = TRUE,
                                update = FALSE) {
  
  annotations <- derive_annotations(sample_io, template, schema, format = "sf", verbose)
  annotations[, expressionUnit := "TPM"]
  annotations[, dataType := "geneExpression"]
  annotations[, sample := NULL]
  if(update) {
    annotate_with_manifest(annotations)
    if(verbose) message("Applied annotations.")
  }
  return(annotations)
  
}


#' Annotate somatic or germline variants output
#' 
#' @inheritParams annotate_aligned_reads
#' @param sample_io Table mapping input to outputs, where outputs are expected to be .vcf.gz files.
#' @param data_type Variant type, given that this can be used with either somatic or germline.
#' @export
annotate_called_variants <- function(sample_io,
                                     template = "bts:ProcessedVariantCallsTemplate", 
                                     schema = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                                     data_type = c("SomaticVariants", "GermlineVariants"),
                                     verbose = TRUE,
                                     update = FALSE) {
  
  annotations <- derive_annotations(sample_io, template, schema, format = "vcf", verbose)
  data_type <- match.arg(data_type)
  annotations[, dataType := data_type]
  annotations[, sample := NULL]
  if(update) {
    annotate_with_manifest(annotations)
    if(verbose) message("Applied annotations.")
  }
  return(annotations)
}



