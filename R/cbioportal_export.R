# Export data from Synapse as a cBioPortal dataset.
# This tries to have the spirit of making a new R package with `devtools`, so dataset is a "package" for cBioPortal.
# This file contain the higher-level wrappers. See `cboilerplate.R` for the true lower-level utils.

#' Enumerate combinations of valid cBP data types
#' 
#' https://docs.cbioportal.org/file-formats/
#' 
#' @keywords internal
cbp_data_types <- function() {
  
  list(c("CLINICAL", "SAMPLE_ATTRIBUTES"),
       c("CLINICAL", "PATIENT_ATTRIBUTES"),
       c("COPY_NUMBER_ALTERATION", "DISCRETE"),
       c("COPY_NUMBER_ALTERATION", "DISCRETE_LONG"),
       c("COPY_NUMBER_ALTERATION", "CONTINUOUS"),
       c("COPY_NUMBER_ALTERATION", "LOG2-VALUE"),
       c("COPY_NUMBER_ALTERATION", "SEG"),
       c("MRNA_EXPRESSION", "CONTINUOUS"),
       c("MRNA_EXPRESSION", "Z-SCORE"),
       c("MUTATION_EXTENDED", "MAF"),
       c("METHYLATION", "CONTINUOUS"),
       c("PROTEIN_LEVEL", "LOG2-VALUE"),
       c("STRUCTURAL_VARIANT", "SV"))
}

#' Create a cBioPortal study
#' 
#' See specifications for a study at https://docs.cbioportal.org/file-formats/#meta-file.
#' 
#' @param cancer_study_identifier Cancer study identifier. See cBioPortal standards for ids.
#' @param name of the cancer study, e.g. something following convention is "Malignant Peripheral Nerve Sheath Tumor (NF-OSI, 2022)".
#' @param type_of_cancer Type of cancer, defaults to "mixed". See also http://oncotree.mskcc.org/#/home.
#' @param name Name of the study.
#' @param description Description of the study. A default generic description is provided.
#' @param citation (Optional) A relevant citation, e.g. "TCGA, Nature 2012".
#' @param pmid (Optional) One or more relevant pubmed ids (comma separated without whitespace); if used, citation cannot be `NULL`.
#' @param groups (Optional) Defaults to "PUBLIC" for use with public cBioPortal; 
#' otherwise, use group names that make sense for your instance.
#' @param add_global_case_list (Optional) Use `NULL` to ignore, but default is `TRUE` for an "All samples" case list to be generated automatically.
#' @param short_name (Optional) Short name for the study.
#' @param publish_dir Directory to create for study if doesn't exist, same as `cancer_study_identifier` by default.
#' @param verbose Verbosity level.
#' 
#' @export
cbp_new_study <- function(cancer_study_identifier,
                          name,
                          type_of_cancer = "mixed",
                          description = "The data are contributed by researchers funded by the Neurofibromatosis Therapeutic Acceleration Program (NTAP). 
                          The reprocessing of the raw data is managed by the NF Open Science Initiative (https://nf.synapse.org/).",
                          citation = NULL,
                          pmid = NULL,
                          groups = "PUBLIC",
                          short_name = NULL,
                          add_global_case_list = TRUE,
                          publish_dir = cancer_study_identifier,
                          verbose = TRUE) {
  
  if(!dir.exists(publish_dir)) {
    if(verbose) message(glue::glue("Creating {publish_dir} dataset directory"))
    dir.create(publish_dir)
  }
  
  message("Setting new dataset directory as working directory.")
  setwd(publish_dir)
  
  df_file <- make_meta_study_generic(cancer_study_identifier = cancer_study_identifier,
                                     type_of_cancer = type_of_cancer, 
                                     name = name, 
                                     description = description, 
                                     citation = citation,
                                     pmid = pmid,
                                     groups = groups, 
                                     short_name = short_name,
                                     add_global_case_list = add_global_case_list)
  
  write_meta(df_file, "meta_study.txt", verbose = verbose)
  if(verbose) message("--- Study meta added ---")
}

# ------------------------------------------------------------------------------- #

#' Export and add clinical data to cBioPortal dataset
#' 
#' #' This should be run in an existing dataset package root.
#' 
#' @param ref_map YAML file specifying the mapping of (NF) clinical metadata to cBioPortal model. See details.
#' @param ref_view A view that contains all clinical data for the study.
cbp_add_clinical <- function(ref_map,
                             ref_view) {
  
  if(verbose) message("--- Pulling the clinical data from Synapse ---")
  df <- get_clinical_data_for_cbp_study(ref_view)
  
  write_cbio_clinical(df, ref_map = ref_map, verbose = verbose)
  
  if(verbose) message("--- Making clinical meta file ---")
  # before making meta, check that data file was actually written, since patient data is optional 
  if(file.exists("data_clinical_patient.txt")) {
    make_meta_patient(cancer_study_identifier, verbose = verbose)
  }
  
  make_meta_sample(cancer_study_identifier, verbose = verbose)
  
  if(verbose) message("Clinical data added.")
  
}

# ------------------------------------------------------------------------------- #

#' Export and add mutations data to cBioPortal dataset
#' 
#' This should be run in an existing dataset package root.
#' 
#' Get merged maf file that represents filtered subset of `maf`s containing only (non-germline) data OK to release publicly.
#' This needs to be packaged with other files like this
#' [example of a public mutations dataset](https://github.com/cBioPortal/datahub/tree/1e03ea6ab5e0ddd497ecf349cbee7d50aeebcd5e/public/msk_ch_2020).
#' 
#' 1. Sanity check that this is the version of the maf release file that we want, based on the samplesheet.
#' Sometimes a later version retracts a sample (`is_releasable`=FALSE) so merged maf being exported should match samplesheet.  
#' Latest versions of the samplesheets tied to each release are currently stored in `syn38793855`.
#' (Note: Please file issue to update this doc if this changes.)
#' 
#' 2. Make the clinical data files. 
#' For NF, the clinical metadata are annotations on the files/surfaced in a view and are pretty basic. 
#' In the future, it would be preferable to store clinical metadata in a real normalized table.
#' For now, this clinical data is pulled in from the view. To map NF clinical variables to the 
#' [cBioPortal dictionary](https://github.com/cBioPortal/clinical-data-dictionary/blob/e9ec08f48bd57aabf193da70cdb5b88bdef5d01d/docs/resource_uri_to_clinical_attribute_mapping.txt)
#' [as recommended](https://docs.cbioportal.org/file-formats/#custom-columns-in-clinical-data), 
#' this step requires a `ref_map`, which is a YAML file.
#' 
#' 3. Make meta files. Meta files are needed for describing the study, mutations data file, clinical data files.
#' 
#' @param merged_maf Synapse id of `merged maf` file for public release.
#' @param samplesheet Synapse id or local path to samplesheet with release info.
#' @param cancer_study_identifier Study identifier, convention is `{tumorType}_{institution}_{year}`, so for example "mpnst_nfosi_2022".
#' @param verbose Whether to provide informative messages throughout.
#' @export
cbp_add_maf <- function(merged_maf,
                        samplesheet,
                        verbose = TRUE) {
  
  .check_login()
  
  if(verbose) message("--- Getting `merged_maf` file ---")
  file <- .syn$get(merged_maf, downloadLocation = ".")
  
  if(verbose) message("--- Standardizing `maf` release data file name ---")
  data_mutations <- sub(file$name, "data_mutations.txt", file$path)
  file.rename(file$path, data_mutations)
  
  if(verbose) message("--- Checking the `maf` release file against samplesheet ---")
  mm <- dt_read(data_mutations_extended)
  ss <- dt_read(samplesheet)
  check_result <- check_maf_release(mm, ss)
  
  if(!is.null(check_result)) stop("Unfortunately, check of `maf` release failed so will not continue.")
  
  if(verbose) message("--- Making maf meta file ---")
  make_meta_maf(cancer_study_identifier, verbose = verbose)
  
  if(verbose) message("Maf data added.")
  
}


#' Check maf file for release
#' 
#' Currently, this is a simple check to make sure released samples are expected. 
#' It may be extended later on as needed. 
#' 
#' @param merged_maf Maf data as a `data.table`.
#' @param samplesheet Samplesheet as a `data.table`.
#' @return Returns `NULL` if everything OK, else the sample ids that don't match expectations.
check_maf_release <- function(merged_maf, 
                              samplesheet) {
  
  # samplesheet[is_releasable == TRUE, .N]
  ss_samples <- samplesheet[is_releasable == TRUE, biospecimen_id]
  mm_samples <- merged_maf[, unique(Tumor_Sample_Barcode)]
  
  result <- NULL
  missing_release <- setdiff(ss_samples, mm_samples)
  no_release <- setdiff(mm_samples, ss_samples)
  
  if(length(missing_release)) {
    test_failed("Maf file seems to be missing samples specified for release.")
    ids_1 <- data.frame(sample = missing_release, type = "missing")
    result <- rbind(result, ids_1)
  }
  if(length(no_release)) {
    test_failed("Maf file contains samples that should not be released!")
    ids_2 <- data.frame(sample = no_release, type = "no_release")
    result <- rbind(result, ids_2)
  }
  if(!length(missing_release) && !length(no_release)) test_passed("Samples for release look as expected.")
  
  invisible(result)
}


#' Match clinical data with maf sample ids
#' 
#' PATIENT_ID and SAMPLE_ID can only contain letters, numbers, points, underscores and/or hyphens.
#' In the nf processing, sample id spaces are replaced with underscores in the `maf`,
#' so this is applied to clinical data to match.
#' 
#' @param clinical_data Clinical data as a `data.table`.
#' @param merged_maf Maf data as a `data.table`.
#' @keywords internal
match_maf_sample_id <- function(clinical_data, merged_maf = NULL) {
  clinical_data$specimenID <- gsub(" ", "_", clinical_data$specimenID)
  # TODO check mafs and clinical data after reformatting ids
  # maf_samples <- unique(merged_maf$Tumor_Sample_Barcode)
  return(clinical_data)
}

# ------------------------------------------------------------------------------- #

#' Export and add CNA (seg) data to cBioPortal dataset
#' 
#' This should be run in an existing dataset package root.
#' 
#' @export
#' 
#' @param cna_release Syn id of CNA release data, expected to be a .seg file.
cbp_add_cna <- function(cna_release) {
  
  if(verbose) message("--- Getting the data file ---")
  file <- .syn$get(cna_release, downloadLocation = ".")
  
  if(verbose) message("--- Making the meta file ---")
  make_meta_cna(...)
} 
