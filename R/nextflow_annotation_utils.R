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
  # Annoyingly, headers are not standard and can use fastq or fastq1 instead of fastq_1
  if("fastq1" %in% names(ss)) {
    setnames(ss, c("fastq1", "fastq2"), c("fastq_1", "fastq_2"))
    message("In this samplesheet version, looks like we have 'fastq1' and 'fastq2' -- reading as 'fastq_1' and 'fastq_2")
  }
  if("fastq" %in% names(ss)) {
    setnames(ss, "fastq", "fastq_1")
    message("In this samplesheet version, looks like we have 'fastq' -- reading as 'fastq_1'")
  }
  
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
#' @return A `data.table` with cols `output_name` `output_id` `sample` `workflow`
#' @export
map_sample_output_rnaseq <- function(syn_out) {
  
  message("Going through the outputs...this may take some seconds.")
  .o <- walk(syn_out)
  l2 <- setnames(rbindlist(.o[[1]][[3]]), c("output_name", "output_id"))[grepl(".bam|.bai", output_name)]
  l2[, sample := gsub("\\.markdup.sorted.*", "", output_name)]
  l3 <- lapply(.o[2:length(.o)], function(x) {
    if("quant.sf" %in% unlist(x)) list(sample = x[[1]][[1]],
                                       output_name = c(x[[3]][[2]][[1]], x[[3]][[3]][[1]]),
                                       output_id = c(x[[3]][[2]][[2]], x[[3]][[3]][[2]]))
  }) %>% rbindlist()
  l3[, sample := gsub("^.*/", "", sample)]
  sample_outputs <- rbind(l2, l3)
  sample_outputs[, workflow := "STAR and Salmon"]
  return(sample_outputs)
}


#' Map sample to output from nf-sarek
#' 
#' See https://nf-co.re/sarek. Most processed outputs are nested by sample and variant callers, i.e. 
#' `*VariantCalling/<TUMOR_vs_NORMAL>/<CALLER>`. Other times the data is organized as
#'  `*VariantCalling/<CALLER>/<TUMOR_vs_NORMAL>`.
#' This walks through the output destination (URI of `*VariantCalling`)
#' with similar intention to \code{\link{map_sample_output_rnaseq}}, but for Sarek outputs.
#' 
#' Note: And additional step post-Sarek will create MAFs in the output subdirectory DeepVariant. 
#' If this is run _after_ the MAF creation step, this will return file indexes with those .maf files.
#' 
#' @param syn_out Syn id of syn output destination with files of interest. 
#' @param sample_level If caller is organized by sample, use 2 (default), if samples organized by caller, use 3. See details. 
#' @import data.table
#' @return A `data.table` with cols `caller` `caller_path` `caller_syn` `output_name` `output_id` `sample` `workflow`
#' @export
map_sample_output_sarek <- function(syn_out, sample_level = 2) {
  
  workflow_level <- if(sample_level == 2) 3 else 2
  
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
  outputs[, sample := sapply(paths, `[[`, sample_level)]
  outputs[, sample := strsplit(sample, "_vs_")]
  outputs[, workflow := sapply(paths, `[[`, workflow_level)]
  return(outputs)
}


#' Map sample input-output
#' 
#' Wrapper to map sample inputs and outputs depending on workflow type. 
#' 
#' @inheritParams map_sample_input_ss
#' @inheritParams map_sample_output_rnaseq
#' @inheritParams map_sample_output_sarek
#' @param workflow Workflow. 
#' @return A table with `sample` `level` `output_id` `output_name` `input_id`.
#' @export
map_sample_io <- function(workflow = c("nf-rnaseq", "nf-sarek"),
                          samplesheet,
                          syn_out,
                          sample_level = 2) {
  
  workflow <- match.arg(workflow)
  sample_inputs <- map_sample_input_ss(samplesheet)
  
  if(workflow == "nf-rnaseq") {
    sample_outputs <- map_sample_output_rnaseq(syn_out)
  } else if(workflow == "nf-sarek") {
    sample_outputs <- map_sample_output_sarek(syn_out, sample_level = sample_level)
    # sample can contain 2 samples (tumor vs normal from same indiv) -> take first
    sample_outputs[, sample := sapply(sample, first)]
  }
  
  # Check that sample ids in sample_outputs are in sample_inputs
  # Presumed OK for sample_inputs to contain samples *not* in outputs 
  # (maybe no outputs if doesn't pass QC checks) but not OK vice versa
  if(!all(unique(sample_outputs$sample) %in% unique(sample_inputs$sample))) {
    stop("Issue found: samples present in outputs are not referenced in inputs.", call. = F)
  }
  sample_io <- merge(sample_outputs, sample_inputs, by = "sample", all.x = TRUE, all.y = FALSE)
  return(sample_io)
}


#' Make annotations from workflow tool stats
#' 
#' Extracts a subset of [samtools stats](http://www.htslib.org/doc/samtools-stats.html)
#' and [picard stats](https://broadinstitute.github.io/picard/picard-metric-definitions.html) 
#' from workflow metafiles to surface as annotations. Note: picard stats only for WGS/WES/targeted sequencing. 
#' Regarding the selection of stats, see the Genomic Data Commons (GDC) model for
#' [Aligned Reads](https://docs.gdc.cancer.gov/Data_Dictionary/viewer/#?view=table-definition-view&id=aligned_reads)  
#' 
#' @param samtools_stats_file Path to file/syn id of file with samtools stats produced by the workflow. 
#' @param picard_stats_file Path to file/syn id of file with picard stats produced by the workflow.
#' @param sample_io Sample input and output mapping, used to assign stats to appropriate outputs (bam)
#' @export
annotate_with_tool_stats <- function(samtools_stats_file = NULL, 
                                     picard_stats_file = NULL,
                                     sample_io = NULL) {
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
  } else { # TO DO
    picard_stats <- dt_read(picard_stats_file)
    picard_stats <- picard_stats[, .(sample = Sample,
                               meanCoverage = MEAN_COVERAGE,
                               proportionCoverage10x = PCT_10X,
                               proportionCoverage30x = PCT_30X)]
  }
  result <- Reduce(function(x, y) merge(x, y, by = "sample"), 
                   Filter(is.data.table, list(sam_stats, picard_stats)))
  if(!is.null(result) && !is.null(sample_io)) {
    result <- merge(result, sample_io[, .(sample, entityId = output_id, output_name)], by = "sample")[grepl(".bam$", output_name)]
    for(col in c("sample", "output_name")) result[[col]] <- NULL
  }
  return(result)
}


#' Derive annotations for processed output data
#' 
#' A processed or derived file can inherit annotations from the input file(s).
#' Currently, this generously facilitates inheritance of many properties except ones that 
#' "obviously" shouldn't be inherited, such as "fileFormat" or "comments". 
#' These rules are hard-coded and might need to be expanded as the data model changes, 
#' and the manifest generated should still be reviewed.
#' 
#' If multiple inputs given, this will inherit annotations from the FIRST input. 
#' Files that pass through this naturally have `dataSubtype` automatically set to "processed" and `fileFormat` set 
#' to the actual new file format. In the future, `template` itself may define default `format` so we don't need to specify explicitly.
#' 
#' @param format File format of the processed data, e.g. "vcf".
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
  if(verbose) message(glue::glue("Creating annotations for ", n, " {format} files"))
  
  props <- get_dependency_from_json_schema(id = template, schema = schema)
  props <- props[!props %in% c("comments", "entityId", "fileFormat", "dataType", "dataSubtype", "progressReportNumber", "Component")]
  
  from <- sapply(x$input_id, `[`, 1)
  to <- x$output_id
  annotations <- Map(function(f, t) copy_annotations(f, t, select = props), from, to) %>% 
    setNames(to) %>% 
    lapply(., reticulate::py_to_r) %>% 
    rbindlist(fill = T, idcol = "entityId")
  
  annotations <- merge(annotations, 
                       x[, .(entityId = output_id, Filename = output_name, workflow)], 
                       by = "entityId")
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
#' 2. Extract metrics from auxiliary files to surface as annotations. See \code{\link{annotate_with_tool_stats}}.
#' 
#' 3. Manually add annotations that can't (yet?) be derived from #1 or #2. Has to be done outside of this util.
#' 
#' Always returns a "partial" manifest, which can be adjusted as needed; 
#' for example, if default values such as the linked workflow version are out of date. 
#' The param `dry_run` specifies whether annotations should be applied.
#' 
#' @inheritParams get_by_prop_from_json_schema
#' @inheritParams annotate_with_tool_stats
#' @param template (Optional) URI of template in data model to use, prefixed if needed. 
#' Can specify different model/version, but in some cases may not work well.
#' @param sample_io Table mapping input to outputs.
#' @param dry_run Whether to apply annotations.
#' @param verbose Give verbose reports for what's happening.
#' @export
annotate_aligned_reads <- function(sample_io,
                                   samtools_stats_file = NULL,
                                   picard_stats_file = NULL,
                                   template = "bts:ProcessedAlignedReadsTemplate",
                                   schema = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                                   verbose = TRUE,
                                   dry_run = TRUE) {
  
  annotations <- derive_annotations(sample_io, template, schema, format = "bam", verbose)
  qc <- annotate_with_tool_stats(samtools_stats_file, picard_stats_file, sample_io)
  if(!is.null(qc)) {
    annotations <- Reduce(function(x, y) merge(x, y, by = "entityId"), 
                          Filter(is.data.table, list(annotations, qc)))
    if(verbose) message("Included some annotations from available workflow metrics.")
  }

  annotations[, dataType := "AlignedReads"]
  if(!dry_run) {
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
                                dry_run = TRUE) {
  
  annotations <- derive_annotations(sample_io, template, schema, format = "sf", verbose)
  annotations[, expressionUnit := "TPM"]
  annotations[, dataType := "geneExpression"]
  if(!dry_run) {
    annotate_with_manifest(annotations)
    if(verbose) message("Applied annotations.")
  }
  return(annotations)
  
}


#' Annotate somatic or germline variants output
#' 
#' Currently, `vcf` are output first and `maf`s appear after a subsequent workflow run, 
#' so depending on when `map_sample_output_sarek` is run to create a `sample_io` file, 
#' there will be just `vcf` files or both. 
#' (In the future, the workflows will likely be joined so both are deposited at the same run.)
#' One can specify to only annotate either the "vcf" or "maf" files and create a manifest
#' for just those files, or use "auto" to detect the file types present in `sample_io`.
#' 
#' `maf` files use the same template but with different default values, such as a subclass term for `dataType`.
#' If in the future `maf`s require a significantly different template, 
#' then this should be factored out into a separate annotation function.
#' 
#' @inheritParams annotate_aligned_reads
#' @param sample_io Table mapping input to outputs, which reference output `.vcf.gz` or `maf` files.
#' @param workflow_ref Character vector with names for workflow and values as version-specific links.
#' @param format Variant format, "auto" to handle any "vcf" or "maf" files present automatically, or specify one explicitly. See details.
#' @param data_type Variant type, use "auto" to infer from naming scheme and current NF processing SOP, or specify more explicitly.
#' @export
annotate_called_variants <- function(sample_io,
                                     workflow_ref,
                                     format = c("auto", "vcf", "maf"),
                                     template = "bts:ProcessedVariantCallsTemplate",
                                     schema = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                                     data_type = c("auto", "SomaticVariants", "GermlineVariants", "AnnotatedSomaticVariants", "AnnotatedGermlineVariants"),
                                     verbose = TRUE,
                                     dry_run = TRUE) {
  
  format <- match.arg(format)
  if(format == "auto") {
    annotations <- list()
    if(any(grepl("vcf[.]gz$", sample_io$output_name))) annotations$vcf <- derive_annotations(sample_io, template, schema, format = "vcf", verbose)
    if(any(grepl("maf$", sample_io$output_name))) annotations$maf <- derive_annotations(sample_io, template, schema, format = "maf", verbose)
    annotations <- rbindlist(annotations)
  } else {
    annotations <- derive_annotations(sample_io, template, schema, format = format, verbose)
  }
  
  data_type <- match.arg(data_type)
  if(data_type == "auto") {
    data_type_assign <- function(name, format) {
      if(grepl("_vs_", name) && format == "vcf") { 
        "SomaticVariants" 
      } else if(!grepl("_vs_", name) && format == "vcf") { # vcfs can be annotated, but this is based on NF processing
        "GermlineVariants" 
      } else if(grepl("_vs_", name) && format == "maf") { 
        "AnnotatedSomaticVariants" 
      } else if(!grepl("_vs_", name) && format == "maf") { 
        "AnnotatedGermlineVariants" 
      } else { 
        stop("Tried to use incompatible values in data assignment rules.") 
      }
    }
  } else {
    data_type_assign <- function(name, format) { data_type }
  }
  annotations[, dataType := data_type_assign(Filename, fileFormat), by = entityId]
  
  annotations[fileFormat == "vcf" & workflow == "Strelka", workflow := "Strelka2"]
  annotations[fileFormat == "vcf", workflowLink := workflow_ref[workflow]]
  
  annotations[fileFormat == "maf", workflow := "nf-vcf2maf"] 
  annotations[fileFormat == "maf", workflowLink := "https://github.com/Sage-Bionetworks-Workflows/nf-vcf2maf/tree/1.0.1"] 
    
  if(!dry_run) {
    annotate_with_manifest(annotations)
    if(verbose) message("Applied annotations.")
  }
  
  return(annotations)
}

#' Annotate CNV outputs from CNVKit
#'
#' @param output_dataset Id of dataset, which should be created first via `nf_cnv_dataset`.
#' @param input_dataset Input dataset used in the workflow.
#' @param dry_run Whether to return a version submit manifest.
#' @param workflow Optional name for workflow, defaults to CNVkit.
#' @param workflowLink Optional corresponding link to workflow, defaults to known used version.
#' @export
annotate_cnv <- function(output_dataset,
                         input_dataset,
                         workflow = "CNVkit",
                         workflowLink = "https://nf-co.re/sarek/3.1/docs/output#cnvkit",
                         dry_run = TRUE) {

  # Set up manifest using output_dataset
  manifest <- table_query(output_dataset, c("id", "name", "parentId"))
  names(manifest) <- c("entityId", "Filename", "parentId")
  manifest <- data.table(manifest)
  manifest[, resourceType := "experimentalData"]
  manifest[, fileFormat := gsub("^.*[.]", "", Filename)]
  manifest[, dataType := "Copy Number Variants"]
  manifest[!fileFormat %in% "bed", dataSubtype := "Copy Number Segments"]
  manifest[fileFormat %in% "bed", dataType := "Reference Data"]
  manifest[, dataType := "Copy Number Variants"]
  manifest[, workflow := workflow]
  manifest[, workflowLink := workflowLink]

  # Create lookup for specimenID given that files are organized by specimenID
  specimenID <- sapply(unique(manifest$parentId), function(x) .syn$get(x, downloadFile = F)$properties$name)
  specimen_lookup <- data.table(parentId = names(specimenID), specimenID = specimenID)
  manifest <- merge(manifest, specimen_lookup, by = "parentId")

  # Bring assay and clinical data over from what's on input_dataset
  meta <- table_query(input_dataset,
                      c("assay", "individualID", "specimenID", "age", "sex",
                        "species", "diagnosis", "tumorType"))
  manifest <- merge(manifest, meta, by = "specimenID")
  manifest$parentId <- NULL
  if(!dry_run) {
    annotate_with_manifest(manifest)
    message("Manifest applied.")
  }
  return(manifest)
}

