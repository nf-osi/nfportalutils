#' Create datasets for Sarek-called somatic or germline variants results
#'
#' Organize variant call files from Nextflow Sarek into 3-4 datasets,
#' grouping files by variant type and workflow with titles having the format:
#' "{type} Genomic Variants - {workflow} Pipeline", e.g. "Somatic Genomic Variants - Strelka Pipeline".
#' As you can see, this assumes that you want to create datasets that segregate Somatic and Germline calls.
#' This makes sense for NF because Germline calls can be treated differently.
#' This uses latest version of all files and creates a Draft version of the dataset.
#'
#' Since we basically just need the syn entity id, variant type, and workflow to group the files.
#' Instead of getting this info through running `map_*` as in the example,
#' you may prefer using a fileview, in which case you just need to download a table from a fileview
#' that has `id` => `output_id` + the `dataType` and `workflow` annotations.
#' The fileview can be used _after_ the files are annotated. If you want to create datasets _before_
#' files are annotated, then you have to use `map_*`.
#'
#' Finally, datasets cannot use the same name if stored in the same project,
#' so if there are multiple batches, the names will have to be made unique by adding
#' the batch number, source data id, processing date, or whatever makes sense.
#'
#' @inheritParams new_dataset
#' @param output_map The `data.table` returned from `map_sample_output_sarek`. See details for alternatives.
#' @param workflow One of workflows used.
#' @param verbose Optional, whether to be verbose -- defaults to TRUE.
#' @import data.table
#' @return A list of dataset objects.
#' @export
#' @examples
#'\dontrun{
#' syn_out <- "syn26648589"
#' m <- map_sample_output_sarek(syn_out)
#' datasets <- nf_sarek_datasets(m, parent = "syn26462036", dry_run = F) # use a test project
#'}
nf_sarek_datasets <- function(output_map,
                              parent,
                              workflow = c("FreeBayes", "Mutect2", "Strelka", "DeepVariant"),
                              verbose = TRUE,
                              dry_run = TRUE) {
  
  output_map <- as.data.table(output_map)
  if(!is.null(output_map$dataType)) {
    data_type <- unique(output_map$dataType)
    if(length(data_type) != 1) stop("Expecting one `dataType`, which does not appear to be the case.")
    gvtype <- grep("(Germline|Somatic)Variants", data_type, value = T)
    if(!length(gvtype)) stop("Data type does not look right, expecting either Germline or Somatic variants.")
    gvtype <- switch(gvtype,
                     SomaticVariants = "Somatic",
                     GermlineVariants = "Germline")
    
  } else {
    # Detect genomic variants type from first path name
    gvtype <- if(grepl("SomaticVariantCalls", first(output_map$caller_path))) {
      "Somatic"
    } else if(grepl("GermlineVariantCalls", first(output_map$caller_path)))  {
      "Germline"
    } else {
      stop("Could not assign either Germline or Somatic labels based on main output folder.
           Check whether folder contains mixed types or is not the right one.")
    }
  }
  pattern <- "vcf.gz(.tbi)?$"
  workflow <- match.arg(workflow)
  datasets <- list()
  for(i in workflow) {
    dataset <- output_map[workflow == i & grepl(pattern, output_name)]
    if(nrow(dataset)) {
      if(verbose) glue::glue("Creating {i} dataset with {nrow(dataset)} files")
      name <- glue::glue("{gvtype} Genomic Variants - {i} Pipeline")
      dataset <- new_dataset(name = name, parent = parent, items = dataset$output_id, dry_run = TRUE)
      if(dry_run) datasets[[i]] <- syn_dataset else datasets[[i]] <- .syn$store(syn_dataset)
    }
  }
  
  return(datasets)
  
}


#' Create dataset for STAR-Salmon expression quantification results
#'
#' With a level-3 manifest that is created from `annotate_expression`,
#' calls `new_dataset` to make quantification files (.sf) into dataset.
#' Uses latest version of the files and creates a "Draft" dataset.
#' See `nf_sarek_datasets`.
#'
#' @inheritParams new_dataset
#' @inheritParams nf_sarek_datasets
#' @param manifest A table of annotated data manifest from `annotate_expression`.
#' @export
nf_star_salmon_datasets <- function(manifest,
                                    parent,
                                    dry_run = TRUE) {
  
  items <- manifest$entityId
  new_dataset(name = "Gene Expression Quantification from RNA-seq",
              parent = parent,
              items = items,
              dry_run = dry_run)
}

#' Create dataset for CNVKit results
#'
#' Create dataset from all files in CNVKit output
#'
#' @inheritParams new_dataset
#' @param syn_out Output folder called 'cnvkit'
#' @export
nf_cnv_dataset <- function(syn_out,
                           parent,
                           dry_run = TRUE) {
  
  files <- walk(syn_out)
  files <- unlist(files)
  df <- as.data.frame(matrix(files, ncol = 2, byrow = TRUE))
  names(df) <- c("Filename", "id")
  df <- df[grepl("cnr$|cns$|cnn$|bed$|pdf$|png$", df$Filename), ]
  items <- df$id
  new_dataset(name = "Copy Number Variant - CNVkit",
              parent = parent,
              items = items,
              dry_run = dry_run)
}