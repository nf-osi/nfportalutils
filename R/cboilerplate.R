# ------------------------------------------------------------------------------ #
# Attribution: The following utils to make cBioPortal files are adapted from some code written by the awesome @hhunterzinck
# in the repo https://github.com/Sage-Bionetworks/genie-erbb2-cbio/

# TO DO: Many of functions for making meta files according to the data type might
# benefit from implementation using S3 classes especially if we will be using more different types in the future,
# but that is currently not worth the rewrite.

# -- DATA FILES ---------------------------------------------------------------- #
# Data files store data... cBioPortal has format specifications specific to the data type. 
# The only data type that we need to script for is the clinical data type, 
# while everything else should already be outputted from the processing pipeline in relatively standard format,
# e.g. `maf`s, gene matrix, etc.

#' Make header for cBioPortal clinical data file
#' 
#' This is called from the wrapper `write_cbio_clinical`.
#' Reused from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_clinical.R#L396.
#' 
#' @param df A `data.frame` representing clinical dataset to publicize. 
#' @param label Character vector representing a short label for each column in the dataset
#' @param description Character vector representing a long descriptions for each column in the dataset
#' @param data_type Character vector representing the data type of each column in the dataset
make_cbio_clinical_header <- function(df, label, description, data_type) {

  # Original code assigns a default priority = 1 to all; this is kept until we need more complex configuration
  header <- rbind(label, description, data_type, rep(1))
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
#' @param clinical_type String representing cBioPortal clinical data type.  
#' @return string
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
#' The patient file is actually optional, so if there are no patient attributes 
#' to create a file for, that is OK.
#' `df` is expected to be a table containing all clinical data available, or maybe even some irrelevant data 
#' (since NF data is not well-normalized and we just have a single table with everything).
#' This relies on a `ref_map` specification to know which clinical data to include for cBioPortal
#' and how to segregate the clinical attributes into the right files. 
#' For example, say `df` contains clinical variables A-X, but mappings are only specified for
#' variables A-C, L-M and others are not meant to be surfaced/made public. This will subset the `df` to what's specified in the mapping.
#' On the other hand, if there is a mapping for variable Z that is _not_ in the clinical data, then this will throw error. 
#' 
#' @inheritParams ref_map
#' @inheritParams make_cbio_clinical_header
#' @inheritParams get_cbio_filename
#' @param na_replace Possible NA values to replace with "" in exported file.
#' @param delim Delimiter character used for writing file, defaults to tab-delimited per cBioPortal specs.
#' @param verbose Whether to be verbose, default TRUE.
#' @export
write_cbio_clinical <- function(df, 
                                ref_map, 
                                na_recode = c("NA", "NaN", "unknown", "", "."),
                                delim = "\t",
                                publish_dir = ".",
                                verbose = TRUE) {
  
  m <- use_ref_map(ref_map)
  m <- split(m, by = "attribute_type")
  if(!"SAMPLE" %in% names(m)) stop("SAMPLE data not present, which is required!")
  for(clinical_type in names(m)) {
    .df <- df[, m[[clinical_type]]$source ]
    .df <- .df %>% recode()
    filename <- get_cbio_filename(clinical_type)
    header <- make_cbio_clinical_header(.df, 
                                        m[[clinical_type]]$label, 
                                        m[[clinical_type]]$description, 
                                        m[[clinical_type]]$data_type)
    
    df_out <- rbind(header, .df)
    path <- glue::glue("{publish_dir}/{filename}")
    write.table(df_out, 
                file = path), 
                sep = delim, 
                na = "",
                col.names = F, 
                row.names = F, 
                quote = F)
    
    if(verbose) message(glue::glue("{clinical_type} data written to: {path}"))
  }
}

# -- META FILES ---------------------------------------------------------------- #
# Meta files describe the data files; they have less variable formatting than the data files.

#' Write meta file
#' 
#' Slightly different implementation than https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L220
#' @keywords internal
write_meta <- function(data, filename, publish_dir = ".", verbose = TRUE) {
  
  path <- glue::glue("{publish_dir}/{filename}")
  writeLines(data, con = path)
  if(verbose) message(glue::glue("Meta file written to: {path}"))
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
  
  rows <- rep("", 4)
  rows[1] <- c(glue::glue("cancer_study_identifier: {cancer_study_identifier}"))
  rows[2] <- c(glue::glue("genetic_alteration_type: {genetic_alteration_type}"))
  rows[3] <- c(glue::glue("datatype: {datatype}"))
  rows[4] <- c(glue::glue("data_filename: {data_filename}"))
  return(rows)
}

#' Make patient meta file
#' 
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L101
#' @inheritParams make_meta_clinical_generic
#' @export
make_meta_patient <- function(cancer_study_identifier, 
                              data_filename = "data_clinical_patient.txt", 
                              verbose = TRUE) {
  
  meta_filename <- "meta_clinical_patient.txt"
  df_file <- make_meta_clinical_generic(cancer_study_identifier = cancer_study_identifier,
                                        genetic_alteration_type = "CLINICAL",
                                        datatype = "PATIENT_ATTRIBUTES",
                                        data_filename = data_filename)
  
  if(write) write_meta(df_file, meta_filename, publish_dir, verbose)
  invisible(df_file)
}

#' Make sample meta file
#' 
#' Adapted from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L109
#' @inheritParams make_meta_clinical_generic
make_meta_sample <- function(cancer_study_identifier, 
                             data_filename = "data_clinical_sample.txt",
                             publish_dir = ".",
                             write = TRUE,
                             verbose = TRUE) {
  
  meta_filename <- "meta_clinical_sample.txt"
  df_file <- make_meta_clinical_generic(cancer_study_identifier = cancer_study_identifier, 
                                        genetic_alteration_type = "CLINICAL",
                                        datatype = "SAMPLE_ATTRIBUTES", 
                                        data_filename = data_filename)
  
  if(write) write_meta(df_file, meta_filename, publish_dir, verbose)
  invisible(df_file)
}

# -- Data meta files ---------------------------------------------------------- #

#' Generic template for genomic-type data file
#'
#' Reused from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/make_meta.R#L65
#' 
#' @inheritParams make_meta_clinical_generic
#' @param stable_id
#' @param profile_name Name of the genomic profiling. This is set by the more specific `make_meta` utility. 
#' For example, "Mutations" for `make_*_maf` and "Copy-number alterations" for `make_*_cna`.  
#' @param profile_description Brief description for the genomic profiling. 
#' This is set by the more specific `make_meta` utility.
#' @keywords internal
make_meta_genomic_generic <- function(cancer_study_identifier,
                                      genetic_alteration_type, 
                                      datatype, 
                                      stable_id, 
                                      profile_name, 
                                      profile_description, 
                                      data_filename) {
  
  rows <- rep(NA, 8)
  rows[1] <- glue::glue("cancer_study_identifier: {cancer_study_identifier}")
  rows[2] <- glue::glue("genetic_alteration_type: {genetic_alteration_type}")
  rows[3] <- glue::glue("datatype: {datatype}")
  rows[4] <- glue::glue("stable_id: {stable_id}")
  rows[5] <- glue::glue("show_profile_in_analysis_tab: true")
  rows[6] <- glue::glue("profile_name: {profile_name}")
  rows[7] <- glue::glue("profile_description: {profile_description}")
  rows[8] <- glue::glue("data_filename: {data_filename}")
  return(rows)
}


#' Make meta file for maf
#' 
#' Reused from https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/make_meta.R#L157
#' 
#' @inheritParams make_meta_genomic_generic
#' @param data_filename Name of the data file. Defaults to a standard name.
#' @export
make_meta_maf <- function(cancer_study_identifier, 
                          data_filename = "data_mutations_extended.txt",
                          publish_dir = ".",
                          write = TRUE) {
  
  meta_filename <- "meta_mutations_extended.txt"
  df_file <- make_meta_genomic_generic(cancer_study_identifier = cancer_study_identifier, 
                                       genetic_alteration_type = "MUTATION_EXTENDED", 
                                       datatype = "MAF", 
                                       stable_id = "mutations",
                                       profile_name = "Mutations",
                                       profile_description = "Mutation data from NF-OSI processing.",
                                       data_filename = data_filename)
  
  if(write) write_meta(df_file, meta_filename, publish_dir, verbose)
  invisible(df_file)
}

# --- Other utils --------------------------------------------------------------#

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
  source <- unlist(sapply(mapping, function(x) x$source))
  label <- unlist(sapply(mapping, function(x) x$target$label))
  description <- unlist(sapply(mapping, function(x) x$target$description))
  data_type <- unlist(sapply(mapping, function(x) x$target$data_type))
  attribute_type <- unlist(sapply(mapping, function(x) x$target$attribute_type))
  if(!all(is.character(source)) && !all(is.character(label)) && !all(is.character(description)) && !all(is.character(data_type)) && !all(is.character(attribute_type))) { 
    stop("Looks like something is missing in the mapping or it is not the structure expected") 
  }
  if(as_dt) {
    ref_map_dt <- data.table(source = source,
                             label = label, 
                             description = description,
                             data_type = data_type,
                             attribute_type = attribute_type)
    return(ref_map_dt)
  } else {
    return(ref_map_ls)
  }
}
