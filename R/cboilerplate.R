# ------------------------------------------------------------------------------ #
# Attribution: Many utils for cBioPortal below are adapted from code written by the awesome @hhunterzinck
# in the repo https://github.com/Sage-Bionetworks/genie-erbb2-cbio/

# TO DO / DEV NOTES:
# 1. Many of functions for making meta files according to the data type might benefit
# from reimplementation using S3 classes esp. if different types explode, but currently not worth the rewrite.

# 2. It may make sense to write meta files automatically whenever writing a
# data file is called. This might be mainly updating the main wrapper or creating more wrappers.


# -- DATA FILES ---------------------------------------------------------------- #
# Data files store data... cBioPortal has format specifications specific to the data type.
# The only data type that we need to script for is the clinical data type,
# while everything else should already be outputted from the processing pipeline in relatively standard format,
# e.g. `maf`s, gene matrix, etc.

#' Make header for cBioPortal clinical data file
#'
#' This is called from the wrapper `write_cbio_clinical`.
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_clinical.R#L396.
#' Needs a data table of clinical data and a reference providing `label`, `description`, and `data_type`.
#'
#' @param df A `data.frame` representing clinical dataset to publicize.
#' @param mapping A reference table providing `label`, `description`, and `data_type` for each `source` attribute in `df`.
#' @keywords internal
make_cbio_clinical_header <- function(df, mapping) {

  label <- mapping[match(names(df), source), label]
  description <- mapping[match(names(df), source), description]
  data_type <- mapping[match(names(df), source), data_type]

  # Original code assigns a default priority = 1 to all; this is kept until we need more complex configuration
  header <- rbind(label, description, data_type, rep(1))
  header <- t(apply(header, 1, function(x) { return(c(paste0("#", x[1]), x[2:length(x)]))}))
  header <- rbind(header, label) # use harmonized name as row-5 attribute names
  rownames(header) <- NULL
  colnames(header) <- colnames(df)
  header <- as.data.frame(header)
  return(header)
}

#' Get cBioPortal clinical file name based on clinical data type
#'
#' This is called from the wrapper `write_cbio_clinical`.
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_clinical.R#L411.
#' Note that of the clinical file types, the PATIENT type can actually be optional,
#' and we (NF) currently don't use the TIMELINE type, so options have been simplified.
#' @param clinical_type String representing cBioPortal clinical data type.
#' @return string
#' @keywords internal
get_cbio_filename <- function(clinical_type = c("SAMPLE", "PATIENT")) {

  clinical_type <- match.arg(clinical_type)
  mapping <- switch(clinical_type,
                    SAMPLE = "data_clinical_sample.txt",
                    PATIENT = "data_clinical_patient.txt")
  return(mapping)
}

#' Write cBioPortal clinical file
#'
#' Wrapper function for creating clinical files. There are two: PATIENT and SAMPLE.
#' The PATIENT file is actually optional, so there are only checks for making sure SAMPLE can be created.
#' `df` is expected to be a table containing clinical data available, and maybe even some irrelevant data
#' (since NF data is not well-normalized and there is a single table with everything).
#'
#' This relies on a `ref_map` specification to know which clinical data to include for cBioPortal
#' and how to segregate the clinical attributes into the right files.
#' For example, say `df` contains clinical variables A-X, but mappings are only specified for
#' variables A-C, L-M and others are not meant to be surfaced/made public. This will subset the `df` to what's specified in the mapping.
#' Conversely, if there is a mapping for variable Z that is _not_ in the clinical data, this _will_ throw error.
#'
#' @inheritParams use_ref_map
#' @inheritParams make_cbio_clinical_header
#' @inheritParams write_meta
#' @param na_recode Possible NA values to replace with a blank string (which seems to be standard) in exported file.
#' @param delim Delimiter character used for writing file, defaults to tab-delimited per cBioPortal specs.
#' @keywords internal
write_cbio_clinical <- function(df,
                                ref_map,
                                na_recode = c("NA", "NaN", "unknown", "Unknown"),
                                delim = "\t",
                                publish_dir = ".",
                                verbose = TRUE) {

  m <- use_ref_map(ref_map)
  present <- names(df)
  required <- m$source[m$required]
  attributes <- unique(m$source)

  # Attribute checks
  message("Clinical attributes present are: ", paste(present, collapse = ", "))
  if(!all(required %in% present)) stop("Missing required clinical element(s):", paste(setdiff(required, present), collapse = ", "))
  if(!all(present %in% attributes)) stop("Missing mapping for:", paste(setdiff(present, attributes), collapse = ","))

  # Take care of list columns and NA
  .df <- data.table::copy(df)
  for(col in names(.df)) {
    if(class(.df[[col]]) == "list") {
      .df[[col]] <- paste(.df[[col]], sep = ",")
      warning(glue::glue("The {col} field was stored as a list has been coerced for export, you may want to check output."), call. = F)
    }
    # Use actual NA's so that `write.table` can write out "" consistently
    .df[.df[[col]] %in% na_recode, col ] <- NA_character_
  }

  files <- list()
  m <- split(m, by = "attribute_type")
  if("individualID" %in% .df) {
    patient_df <- unique(.df)[, c(names(.df) %in% m$PATIENT$source)]
    header <- make_cbio_clinical_header(patient_df, m$PATIENT)
    patient_df <- rbind(header, patient_df)
    files[["PATIENT"]] <- patient_df
  }
  {
    sample_df <- .df[, c(names(.df) %in% m$SAMPLE$source)]
    header <- make_cbio_clinical_header(sample_df, m$SAMPLE)
    sample_df <- rbind(header, sample_df)
    files[["SAMPLE"]] <- sample_df
  }

  for(clinical_type in names(files)) {
    filename <- get_cbio_filename(clinical_type)
    path <- glue::glue("{publish_dir}/{filename}")
    write.table(files[[clinical_type]],
                file = path,
                sep = delim,
                na = "",
                col.names = F,
                row.names = F,
                quote = F)
    if(verbose) message(glue::glue("{clinical_type} data written to: {path}"))
  }

  invisible(files)
}

# -- META FILES ---------------------------------------------------------------- #
# Meta files describe the data files; they have less variable formatting than the data files.

#' Write meta file
#'
#' Slightly different implementation than https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L220
#'
#' @param data The data (lines) to write.
#' @param filename Name of file.
#' @param publish_dir Directory path to write to, defaults to current.
#' @param verbose Report where file has been written.
#' @keywords internal
write_meta <- function(data,
                       filename,
                       publish_dir = ".",
                       verbose = TRUE) {

  path <- glue::glue("{publish_dir}/{filename}")
  writeLines(data, con = path)
  if(verbose) checked_message(glue::glue("Meta file written to: {path}"))
}

# -- Clinical meta files ------------------------------------------------------- #
#' Generic template for clinical data file
#'
#' Make meta file to describe one of the clinical data files (e.g. SAMPLE, PATIENT).
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/make_meta.R#L65
#'
#' @param cancer_study_identifier The study identifier.
#' @param genetic_alteration_type The cBioPortal generic alteration type.
#' @param datatype The cBioPortal data type of `data_filename`.
#' @param data_filename Name of the data file that this meta file describes.
#' @keywords internal
make_meta_clinical_generic <- function(cancer_study_identifier,
                                       genetic_alteration_type,
                                       datatype,
                                       data_filename) {

  meta <- glue::glue("cancer_study_identifier: {cancer_study_identifier}") %>%
    append_kv("genetic_alteration_type", genetic_alteration_type) %>%
    append_kv("datatype", datatype) %>%
    append_kv("data_filename", data_filename) %>%

    return(meta)
}

#' Make patient meta file
#'
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L101
#' @inheritParams make_meta_clinical_generic
#' @inheritParams write_meta
#' @param write Whether to write the meta file for the clinical data file.
#' @keywords internal
make_meta_patient <- function(cancer_study_identifier,
                              data_filename = "data_clinical_patient.txt",
                              write = TRUE,
                              publish_dir = ".",
                              verbose = TRUE) {

  df_file <- make_meta_clinical_generic(cancer_study_identifier = cancer_study_identifier,
                                        genetic_alteration_type = "CLINICAL",
                                        datatype = "PATIENT_ATTRIBUTES",
                                        data_filename = data_filename)

  if(write) write_meta(df_file, "meta_clinical_patient.txt", publish_dir, verbose)
  invisible(df_file)
}

#' Make sample meta file
#'
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L109
#' @inheritParams make_meta_clinical_generic
#' @inheritParams make_meta_patient
#' @inheritParams write_meta
#' @keywords internal
make_meta_sample <- function(cancer_study_identifier,
                             data_filename = "data_clinical_sample.txt",
                             publish_dir = ".",
                             write = TRUE,
                             verbose = TRUE) {

  df_file <- make_meta_clinical_generic(cancer_study_identifier = cancer_study_identifier,
                                        genetic_alteration_type = "CLINICAL",
                                        datatype = "SAMPLE_ATTRIBUTES",
                                        data_filename = data_filename)

  if(write) write_meta(df_file, "meta_clinical_sample.txt", publish_dir, verbose)
  invisible(df_file)
}

# -- Data meta files ---------------------------------------------------------- #

#' Append key-value pair dependent on value being given
#'
#' @param quote Quote the value (needed if contains conflict YAML characters).
#' @keywords internal
append_kv <- function(x, key, value, quote = FALSE) {

  if(quote) value <- shQuote(value, type = "sh")
  if(!is.null(value)) append(x, glue::glue("{key}: {value}")) else x
}

#' Generic template for genomic-type data file
#'
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/make_meta.R#L65
#' Internal workhorse union of _all_ the properties used for a genomic-type data file --
#' the sensible defaults/specific combination should be passed in by a higher-level fun, e.g. `make_meta_maf`.
#'
#' @inheritParams make_meta_clinical_generic
#' @param stable_id Stable id.
#' @param reference_genome_id Reference genome id, e.g. 'hg19'.
#' @param profile_name Name of the genomic profiling. This is set by the more specific `make_meta` utility.
#' For example, "Mutations" for `make_*_maf` and "Copy-number alterations" for `make_*_cna`.
#' @param profile_description Brief description for the genomic profiling.
#' This is set by the more specific `make_meta` utility.
#' @keywords internal
make_meta_genomic_generic <- function(cancer_study_identifier,
                                      genetic_alteration_type,
                                      datatype,
                                      stable_id = NULL,
                                      reference_genome_id = NULL,
                                      profile_name = NULL,
                                      profile_description = NULL,
                                      data_filename) {


  meta <- glue::glue("cancer_study_identifier: {cancer_study_identifier}") %>%
    append_kv("genetic_alteration_type", genetic_alteration_type) %>%
    append_kv("datatype", datatype) %>%
    append_kv("reference_genome_id", reference_genome_id) %>%
    append_kv("stable_id", stable_id) %>%
    append_kv("show_profile_in_analysis_tab", "true") %>%
    append_kv("profile_name", profile_name) %>%
    append_kv("profile_description", profile_description) %>%
    append_kv("data_filename", data_filename)

  return(meta)
}


#' Make meta file for maf
#'
#' Reused from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/make_meta.R#L157
#'
#' @inheritParams make_meta_genomic_generic
#' @inheritParams write_meta
#' @param data_filename Name of the data file. Defaults to "data_mutations.txt".
#' @param write Whether to write the meta file for the data file.
#' @keywords internal
make_meta_maf <- function(cancer_study_identifier,
                          data_filename = "data_mutations.txt",
                          publish_dir = ".",
                          write = TRUE,
                          verbose = TRUE) {

  df_file <- make_meta_genomic_generic(cancer_study_identifier = cancer_study_identifier,
                                       genetic_alteration_type = "MUTATION_EXTENDED",
                                       datatype = "MAF",
                                       stable_id = "mutations",
                                       profile_name = "Mutations",
                                       profile_description = "Mutation data from NF-OSI processing.",
                                       data_filename = data_filename)

  if(write) write_meta(df_file, "meta_mutations.txt", publish_dir, verbose)
  invisible(df_file)
}


#' Make meta file for cBioPortal copy number alteration data
#'
#' Currently assumes seg data and should be extended later.
#'
#' See https://docs.cbioportal.org/file-formats/#segmented-data
#' @keywords internal
make_meta_cna <- function(cancer_study_identifier,
                          data_filename = "data_cna.seg",
                          reference_genome_id = "hg19",
                          publish_dir = ".",
                          write = TRUE,
                          verbose = TRUE) {

  df_file <- make_meta_genomic_generic(cancer_study_identifier = cancer_study_identifier,
                                       genetic_alteration_type = "COPY_NUMBER_ALTERATION",
                                       datatype = "SEG",
                                       reference_genome_id = reference_genome_id,
                                       profile_description = "Somatic CNA from NF-OSI processing.",
                                       data_filename = data_filename)

  if(write) write_meta(df_file, "meta_seg.txt", publish_dir, verbose)
  invisible(df_file)

}


#' Make meta file for cBioPortal expression data
#'
#' https://docs.cbioportal.org/file-formats/#expression-data
#' @keywords internal
make_meta_expression <- function(cancer_study_identifier,
                                 type = "raw",
                                 data_filename = glue::glue("data_expression_{type}.txt"),
                                 publish_dir = ".",
                                 write = TRUE,
                                 verbose = TRUE) {

  profile_name <- "mRNA expression"
  profile_description <- "Expression levels"

  if (type == "raw") {
    stable_id <- "rna_seq_mrna"
  } else if(type == "tpm") {
    stable_id <- "mrna_seq_tpm"
    profile_name <- "Normalized gene expression"
    profile_description <- "Normalized gene expression levels (TPM)"
  } else  {
    stable_id <- "rna_seq_mrna"
    message(glue::glue("By default, type will be mapped to `stable_id: {stable_id}`. "),
                       "If you'd like more options, update the file manually or submit a feature request.")
  }

  df_file <- make_meta_genomic_generic(cancer_study_identifier = cancer_study_identifier,
                                       genetic_alteration_type = "MRNA_EXPRESSION",
                                       datatype = "CONTINUOUS",
                                       stable_id = stable_id,
                                       profile_name = profile_name,
                                       profile_description = profile_description,
                                       data_filename = data_filename)

  if(write) write_meta(df_file, glue::glue("meta_expression_{type}.txt"), publish_dir, verbose)
  invisible(df_file)

}


# --- Meta study --------------------------------------------------------------- #

#' Template for meta study file
#'
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L90
#' Low-level internal function for the tedious templating.
#'
#' @keywords internal
make_meta_study_generic <- function(cancer_study_identifier,
                                    type_of_cancer,
                                    name,
                                    description,
                                    citation = NULL,
                                    pmid = NULL,
                                    groups = NULL,
                                    short_name = NULL,
                                    add_global_case_list = TRUE) {

  # Check meta params -- there probably should just be JSON schemas for all of these meta configs
  if(!is.null(pmid) && is.null(citation)) stop("If `pmid` is used, `citation` has to be filled in.")
  if(!is.null(add_global_case_list) && !is.logical(add_global_case_list)) stop("Nonsensical value used for `add_global_case_list`.")

  meta <- glue::glue("cancer_study_identifier: {cancer_study_identifier}") %>%
    append_kv("type_of_cancer", type_of_cancer) %>%
    append_kv("name", name, quote = TRUE) %>%
    append_kv("description", description) %>%
    append_kv("citation", citation, quote = TRUE) %>%
    append_kv("pmid", pmid, quote = TRUE) %>%
    append_kv("groups", groups) %>%
    append_kv("short_name", short_name) %>%
    append_kv("add_global_case_list", tolower(as.character(add_global_case_list)))

  return(meta)
}


# --- Other utils -------------------------------------------------------------- #

#' Read and use a mapping file
#'
#' The mapping file should be a YAML or JSON format file that minimally has a mapping key
#' storing the translations of source data model element to the target data model.
#' See [example file for NF](https://github.com/nf-osi/nf-metadata-dictionary/tree/main/mappings).
#' This util reads the mapping file, does some light checking, and creates a list object that downstream functions can use.
#'
#' If the specification for the mapping file is later formalized with a different structure
#' this should be handled accordingly (that is to say, this is one of the under-the-hood things that can change).
#'
#' @param ref_map YAML or JSON mapping. See details.
#' @param as_dt Return as `data.table`, the default,
#' otherwise do checking but just return the list representation, which retains some metadata.
#' @return Either a list of lists storing `source`, `label`, `description`, `data_type`, `attribute_type`
#' or a `data.table` representation.
#' @keywords internal
use_ref_map <- function(ref_map, as_dt = TRUE) {

  ref_map_ls <- yaml::read_yaml(ref_map) # this can read JSON
  mapping <- ref_map_ls$mapping

  if(as_dt) {
    ref_map_dt <- as.data.table(plyr::ldply(mapping, data.frame, stringsAsFactors=FALSE))
    names(ref_map_dt) <- gsub("target.", "", names(ref_map_dt), fixed = TRUE)
    return(ref_map_dt)
  } else {
    return(ref_map_ls)
  }
}
