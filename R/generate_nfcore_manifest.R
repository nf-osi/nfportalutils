#' Generate nf-core samplesheet manifest
#'
#' @description Generate a samplesheet manifest for nf-core workflows (rnaseq or sarek) 
#' from Synapse dataset metadata. The function queries the dataset metadata table and 
#' reformats it into the appropriate samplesheet format where each row represents a 
#' biological sample that may have multiple associated files.
#'
#' @param dataset_id Synapse ID of the dataset to query
#' @param workflow_name Name of the nf-core workflow ("rnaseq" or "sarek")
#' @param synstage Logical indicating whether to use Synapse staging for file paths
#' @param output_file Optional path to save the samplesheet CSV file. If NULL, returns the data frame.
#' @param strandedness For rnaseq workflow, strandedness setting ("forward", "reverse", "unstranded", or "auto"). Defaults to "auto".
#'
#' @return A data frame with the samplesheet format appropriate for the specified workflow
#'
#' @examples
#' \dontrun{
#' # Generate rnaseq samplesheet
#' rnaseq_manifest <- generate_nfcore_manifest(
#'   dataset_id = "syn12345678",
#'   workflow_name = "rnaseq",
#'   synstage = TRUE
#' )
#' 
#' # Generate sarek samplesheet
#' sarek_manifest <- generate_nfcore_manifest(
#'   dataset_id = "syn12345678", 
#'   workflow_name = "sarek",
#'   synstage = FALSE
#' )
#' }
#'
#' @export
generate_nfcore_manifest <- function(dataset_id, 
                                   workflow_name, 
                                   synstage = FALSE,
                                   output_file = NULL,
                                   strandedness = "auto") {
  
  # Check login
  .check_login()
  
  # Validate workflow name
  workflow_name <- tolower(workflow_name)
  if (!workflow_name %in% c("rnaseq", "sarek")) {
    stop("workflow_name must be either 'rnaseq' or 'sarek'")
  }
  
  # Validate strandedness for rnaseq
  if (workflow_name == "rnaseq" && !strandedness %in% c("forward", "reverse", "unstranded", "auto")) {
    stop("strandedness must be one of: 'forward', 'reverse', 'unstranded', 'auto'")
  }
  
  # Query the dataset metadata
  message(paste("Querying dataset", dataset_id, "for", workflow_name, "samplesheet generation..."))
  metadata <- table_query(dataset_id)
  
  # Check if required columns exist
  required_cols <- c("individualID", "specimenID", "name", "id")
  missing_cols <- required_cols[!required_cols %in% names(metadata)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Generate samplesheet based on workflow
  if (workflow_name == "rnaseq") {
    samplesheet <- .generate_rnaseq_samplesheet(metadata, synstage, strandedness, dataset_id)
  } else if (workflow_name == "sarek") {
    warning("Sarek samplesheet generation has not been fully tested yet. Please verify the output format.")
    samplesheet <- .generate_sarek_samplesheet(metadata, synstage, dataset_id)
  }
  
  # Save to file if specified
  if (!is.null(output_file)) {
    readr::write_csv(samplesheet, output_file)
    message(paste("Samplesheet saved to:", output_file))
  }
  
  return(samplesheet)
}

#' Generate rnaseq samplesheet format
#' @param metadata Data frame with dataset metadata
#' @param synstage Logical for Synapse staging
#' @param strandedness Strandedness setting
#' @param dataset_id Synapse dataset ID for staging
#' @return Data frame in rnaseq samplesheet format
#' @keywords internal
.generate_rnaseq_samplesheet <- function(metadata, synstage, strandedness, dataset_id) {
  
  # Extract sample information and pair R1/R2 files
  samplesheet <- metadata %>%
    dplyr::mutate(
      # Extract sample name from specimenID (e.g., "NF0017-T2-organoids" -> "NF0017-T2-organoids")
      sample = specimenID,
      # Determine if file is R1 or R2 based on filename
      read_type = dplyr::case_when(
        grepl("-R1\\.fastq\\.gz$", name) ~ "R1",
        grepl("-R2\\.fastq\\.gz$", name) ~ "R2",
        TRUE ~ "unknown"
      ),
      # Create file path based on synstage setting
      file_path = ifelse(synstage, paste0("syn://", id), name)
    ) %>%
    tidyr::pivot_wider(
      names_from = read_type,
      values_from = file_path,
      names_prefix = "fastq_",
      values_fn = list
    ) %>%
    dplyr::mutate(
      # Collapse multiple files into comma-separated strings
      fastq_1 = purrr::map_chr(fastq_R1, ~ if(is.null(.x)) "" else paste(.x, collapse = ",")),
      fastq_2 = purrr::map_chr(fastq_R2, ~ if(is.null(.x)) "" else paste(.x, collapse = ",")),
      strandedness = strandedness
    ) %>%
    dplyr::select(sample, fastq_1, fastq_2, strandedness)
  
  return(samplesheet)
}

#' Generate sarek samplesheet format  
#' @param metadata Data frame with dataset metadata
#' @param synstage Logical for Synapse staging
#' @param dataset_id Synapse dataset ID for staging
#' @return Data frame in sarek samplesheet format
#' @keywords internal
.generate_sarek_samplesheet <- function(metadata, synstage, dataset_id) {
  
  # Extract sample information and pair R1/R2 files
  samplesheet <- metadata %>%
    dplyr::mutate(
      # Use individualID as subject
      subject = individualID,
      # Use specimenID as sample
      sample = specimenID,
      # Extract lane from readPair column
      lane = readPair,
      # Determine sample type based on specimenID patterns
      type = dplyr::case_when(
        stringr::str_detect(specimenID, "(?i)skin") ~ "normal",
        stringr::str_detect(specimenID, "(?i)t[0-9]") ~ "tumor",
        stringr::str_detect(specimenID, "(?i)organoids") ~ "tumor",
        TRUE ~ "unknown"
      ),
      # Determine if file is R1 or R2 based on filename
      read_type = dplyr::case_when(
        grepl("_1\\.fq\\.gz$", name) ~ "R1",
        grepl("_2\\.fq\\.gz$", name) ~ "R2",
        TRUE ~ "unknown"
      ),
      # Create file path based on synstage setting
      file_path = ifelse(synstage, paste0("syn://", id), name)
    ) %>%
    tidyr::pivot_wider(
      names_from = read_type,
      values_from = file_path,
      names_prefix = "fastq_",
      values_fn = list
    ) %>%
    dplyr::mutate(
      # Collapse multiple files into comma-separated strings
      fastq_1 = purrr::map_chr(fastq_R1, ~ if(is.null(.x)) "" else paste(.x, collapse = ",")),
      fastq_2 = purrr::map_chr(fastq_R2, ~ if(is.null(.x)) "" else paste(.x, collapse = ",")),
      bam = "",  # Optional field, left empty
      umi = ""   # Optional field, left empty
    ) %>%
    dplyr::select(subject, sample, lane, fastq_1, fastq_2, bam, umi, type)
  
  return(samplesheet)
}
