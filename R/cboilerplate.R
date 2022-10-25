# ------------------------------------------------------------------------------ #
# Attribution: The following utils to make cBioPortal files are adapted from some code written by @hhunterzinck
# in the repo https://github.com/Sage-Bionetworks/genie-erbb2-cbio/
# ------------------------------------------------------------------------------ #

# TO DO: Many of functions for making meta files according to the data type could 
# benefit from implementation using S3 classes.

# -- DATA FILES ---------------------------------------------------------------- #
# Data files store data... cBioPortal has format specifications specific to the data type. 
# The only data type that we need to script for is the clinical data type, 
# while everything else should be outputted from the processing pipeline in relatively standard formats,
# e.g. `maf`s, gene matrix, etc.

#' Make header for cBioPortal clinical data file
#' 
#' This is called from the wrapper `write_cbio_clinical`.
#' Refactored from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_clinical.R#L396.
#' @param df `data.frame` representing clinical dataset
#' @param meta_mapping Either a list of storing information for clinical mappings or 
#' path to the YAML file describing the clinical mappings. 
#' For each clinical variable, it will reference the `label`, `description`, and `type` fields.
make_cbio_clinical_header <- function(df, meta_mapping) {
  
  meta_mapping <- ""
  header <- rbind(label, description, type, rep(1))
  header <- t(apply(header, 1, function(x) { return(c(paste0("#", x[1]), x[2:length(x)]))}))
  header <- rbind(header, colnames(df))
  rownames(header) <- NULL
  colnames(header) <- colnames(df)
  return(header)
}

#' Get cBioPortal clinical file name based on clinical data type
#' 
#' This is called from the wrapper `write_cbio_clinical`.
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_clinical.R#L411.
#' Note that of the clinical file types, the PATIENT type can actually be optional, 
#' and we (NF) currently don't use the TIMELINE type, so options have been simplified.
#' @param sample_type String representing cBioPortal clinical data type.  
#' @return string
get_cbio_filename <- function(sample_type = c("SAMPLE", "PATIENT")) {
  
  sample_type <- match.arg(sample_type)
  mapping <- switch(sample_type,
                    SAMPLE = "data_clinical_sample.txt",
                    PATIENT = "data_clinical_sample.txt")
  return(mapping)
}

#' Write cBioPortal clinical file 
#' 
#' Wrapper function.
#' 
#' @inheritParams make_cbio_clinical_header
#' @inheritParams get_cbio_filename
#' @param delim Delimiter character used for writing file, defaults to tab-delimited per cBioPortal specs.
#' @return File name of output file
#' @export
write_cbio_clinical <- function(df, 
                                sample_type,
                                meta_mapping, 
                                delim = "\t") {
  
  filename <- get_cbio_filename(sample_type)
  header <- make_cbio_clinical_header(df, meta_mapping)
  
  df_out <- rbind(header, df)
  write.table(df_out, 
              file = filename, 
              sep = delim, 
              na = "",
              col.names = F, 
              row.names = F, 
              quote = F)
  
  return(filename)
}

# -- META FILES ---------------------------------------------------------------- #
# Meta files describe the data files; they have less variable formatting than the data files.


#' Generic template for clinical data file
#' 
#' Make meta file to describe one of the clinical data files (e.g. SAMPLE, PATIENT).
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L65
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
  
  rows <- rep("", 4)
  rows[1] <- c(glue::glue("cancer_study_identifier: {cancer_study_identifier}"))
  rows[2] <- c(glue::glue("genetic_alteration_type: {genetic_alteration_type}"))
  rows[3] <- c(glue::glue("datatype: {datatype}"))
  rows[4] <- c(glue::glue("data_filename: {data_filename}"))
  return(rows)
}

#' Generic template for genomic-type data file
#'
#' Reused from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L65
#' 
#' @inheritParams make_meta_clinical_generic
#' @param stable_id
#' @param profile_name Name of the genomic profiling. This is set by the more specific `make_meta` utility. 
#' For example, "Mutations" for `make_*_maf` and "Copy-number alterations" for `make_*_cna`.  
#' @param profile_description Brief description for the genomic profiling. 
#' This is set by the more specific `make_meta` utility.
make_meta_genomic_generic <- function(cancer_study_identifier,
                                      genetic_alteration_type, 
                                      datatype, 
                                      stable_id, 
                                      profile_name, 
                                      profile_description, 
                                      data_filename) {
  
  rows <- rep(NA, 8)
  rows[1] <- glue("cancer_study_identifier: {cancer_study_identifier}")
  rows[2] <- glue("genetic_alteration_type: {genetic_alteration_type}")
  rows[3] <- glue("datatype: {datatype}")
  rows[4] <- glue("stable_id: {stable_id}")
  rows[5] <- glue("show_profile_in_analysis_tab: true")
  rows[6] <- glue("profile_name: {profile_name}")
  rows[7] <- glue("profile_description: {profile_description}")
  rows[8] <- glue("data_filename: {data_filename}")
  return(rows)
}


#' Make meta file for maf
#' 
#' Reused from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L157
#' 
#' @inheritParams make_meta_genomic_generic
#' @param data_filename Name of the data file. Defaults to a standard name.
#' @export
make_meta_maf <- function(cancer_study_identifier, 
                          data_filename = "data_mutations_extended.txt") {
  df_file <- create_meta_genomic_generic(cancer_study_identifier = cancer_study_identifier, 
                                         genetic_alteration_type = "MUTATION_EXTENDED", 
                                         datatype = "MAF", 
                                         stable_id = "mutations",
                                         profile_name = "Mutations",
                                         profile_description = "Mutation data from NF-OSI processing.",
                                         data_filename = data_filename)
  return(df_file)
}

