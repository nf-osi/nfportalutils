#' Translate output folders to workflow context specific metadata
#' 
#' This is a manually maintained reference that covers nextflow outputs for 
#' https://nf-co.re/sarek/{version}/docs/output/#qc-and-reporting & https://nf-co.re/rnaseq/{version}/docs/output/#quality-control,
#' where reference should be accurate for rnaseq version = 3.x.x and sarek version = 2.x.x - 3.x.x.
#'
#' @param output_name Name of the output directory.
#' @param workflow_context Supported nextflow workflow context, "rnaseq" or "sarek".
#' @import data.table
#' @keywords internal
get_workflow_ref <- function(output_name, workflow_context) {
  
  # Continue down the folder tree; if folder name matches the below,
  # create a manifest and annotate the data, otherwise go down if there are any child folders
  
  # Known output folders <-> official name <-> workflow-specific URI fragment <-> specialized processing function if any
  known_outputs <- rbindlist(list(
    list("pipeline_info", "Nextflow", "#pipeline-information", "#pipeline-information", "workflow metadata"),
    list("fastqc", "FastQC", "#fastqc", "#fastqc", "workflow report"), 
    list("multiqc", "MultiQC", "#multiqc", "#multiqc", "workflow report"),
    list("samtools", "Samtools", "#samtools", "#samtools-stats", "workflow report"), 
    list("markduplicates", "GATK MarkDuplicates", "#mark-duplicates", NA, "experimentalData"),
    # list("markduplicates", "GATK MarkDuplicates", "#picard-markduplicates", NA, "workflow report"),
    list("duplicatesmarked", "GATK MarkDuplicates", NA, "#mark-duplicates", "experimentalData"), # preprocessed bam files stored here
    list("recalibrated", "GATK BaseRecalibration", NA, "#base-quality-score-recalibration", "experimentalData"), # preprocessed bam files stored here
    list("recal_table", "GATK BaseRecalibration", NA, "#base-quality-score-recalibration", "workflow report"), # preprocessed bam files stored here
    list("picard_metrics", "GATK MarkDuplicates", "#picard-markduplicates", NA, "workflow report"),
    list("snpeff", "snpEff", NA, "#snpeff-reports", "workflow report"), 
    list("vep", "VEP", NA, "#vep-reports", "workflow report"), 
    list("trimgalore", "TrimGalore", "#trimgalore", NA, "workflow report"),
    list("rseqc", "RSeQC", "#rseqc", NA, "workflow report"),
    list("qualimap", "Qualimap", "#qualimap", NA, "workflow report"),
    list("dupradar", "dupRadar", "#dupradar", NA, "workflow report"),
    list("preseq", "Preseq", "#preseq", NA, "workflow report"),
    list("featurecounts", "featureCounts", "#featurecounts", NA, "workflow report"),
    list("deseq2", "DESeq2", "#deseq2", NA, "workflow report"),
    list("stringtie", "StringTie", "#stringtie", NA, "experimentalData"),
    list("bamqc", "bamQC", NA, "#bamqc", "workflow report"),
    list("bigwig", "bedGraphToBigWig", "#bedtools-and-bedgraphtobigwig", NA, "experimentalData"),
    list("bcftoolsstats", "BCFToolsStats", NA, "#bcftools-stats", "workflow report"),
    list("bcftools", "BCFTools", NA, "#bcftools", "workflow report"),
    list("fastp", "FastP", NA, "#fastp", "workflow report"),
    list("mosdepth", "Mosdepth", NA, "#mosdepth", "workflow report"),
    list("vcftools", "VCFtools", NA, "#vcftools", "workflow report")
    #list("deepvariant", "DeepVariant", NA, "#deepvariant"),
    #list("freebayes", NA, NA, NA)
    ))
  setnames(known_outputs, c("folder_name", "official_name", "rnaseq", "sarek", "resource_type"))
  
  output_match <- known_outputs[folder_name %in% output_name, 
                                .(resourceType = resource_type, workflow = official_name, workflowLink = get(workflow_context))]
  if(!nrow(output_match)) return()
  return(as.list(output_match))
}


#' Recursive annotation of nextflow outputs
#'
#' @param output One or more nextflow output folders, must be a named vector. 
#' This can be a single top-level `publishDir` to automate annotations for all outputs, or a couple of folders for more selective annotation.
#' @param workflow_ref The base workflow reference used for `workflowLink`. If given top-level folder, this can be extracted automatically, otherwise must be passed in.
#' @param assay Assay related to data outputs.
#' @export
annotate_auxiliary <- function(output,
                               workflow_ref, 
                               assay = "whole exome sequencing") {
  
  # Get workflow_ref 
  
  # Extract workflow
  workflow_context <- "sarek"
  
  here_id <- output[1]
  here_name <- tolower(names(output)[1])
  output_match <- get_workflow_ref(here_name, workflow_context)
  
  if(!is.null(output_match)) {
    
    manifest <- manifest_make(here_id)
    manifest$resourceType <- output_match$resourceType
    manifest$assay <- assay
    manifest$fileFormat <- sapply(manifest$Filename, asFileFormat)
    manifest$dataType <- sapply(manifest$Filename, asDataType)
    manifest$dataSubtype <- "processed"
    manifest$workflow <- output_match$workflow
    manifest$workflowLink <- paste0(workflow_ref, output_match$workflowLink)
    invisible(annotate_with_manifest(manifest))
    message(glue::glue("Submitted annotations for {here_name} ({here_id})"))
    output <- output[-1]
    
    if(length(output)) {
      annotate_auxiliary(output, workflow_ref, assay)
    } else {
      message("Done with all!")
    }
    
  } else {
    
    more <- find_child_type(here_id, list("folder"))
    output <- c(more, output[-1])
    annotate_auxiliary(output, workflow_ref, assay)
    
  }
}

