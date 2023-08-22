# Export data from Synapse as a cBioPortal dataset, where different data types can be added to the package one-by-one,
# much in the spirit of https://github.com/r-lib/usethis.
# This file contains the higher-level wrappers that are exported; see `cboilerplate.R` for the non-exported lower-level utils.
# All functions should start with `cbp_*` so that it's clear this is cBioPortal-relevant functionality.

#' Enumerate combinations of valid cBP data types and data subtypes
#' 
#' https://docs.cbioportal.org/file-formats/
#' 
#' @keywords internal
cbp_datatypes <- function() {

  types <- data.table::rbindlist(list(
    list("CLINICAL", "SAMPLE_ATTRIBUTES"),
    list("CLINICAL", "PATIENT_ATTRIBUTES"),
    list("COPY_NUMBER_ALTERATION", "DISCRETE"),
    list("COPY_NUMBER_ALTERATION", "DISCRETE_LONG"),
    list("COPY_NUMBER_ALTERATION", "CONTINUOUS"),
    list("COPY_NUMBER_ALTERATION", "LOG2-VALUE"),
    list("COPY_NUMBER_ALTERATION", "SEG"),
    list("MRNA_EXPRESSION", "CONTINUOUS"),
    list("MRNA_EXPRESSION", "Z-SCORE"),
    list("MUTATION_EXTENDED", "MAF"),
    list("METHYLATION", "CONTINUOUS"),
    list("PROTEIN_LEVEL", "LOG2-VALUE"),
    list("STRUCTURAL_VARIANT", "SV")))
  
  setnames(types, c("dataType", "dataSubtype"))
  return(types)
  }

#' Initialize a new cBioPortal study dataset
#' 
#' Create a new directory with a basic required [study meta file](https://docs.cbioportal.org/file-formats/#meta-file),
#' much like how we'd create a new R package and put a DESCRIPTION file in it.
#' 
#' @param cancer_study_identifier Cancer study identifier in format such as `nst_nfosi_ntap_2022`.
#' @param name Name of the study, e.g. "Malignant Peripheral Nerve Sheath Tumor (NF-OSI, 2022)".
#' @param type_of_cancer Type of cancer, see http://oncotree.mskcc.org/#/home.
#' @param description Description of the study, defaults to a generic description that can be edited later.
#' @param short_name (Optional) Short name for the study.
#' @param citation (Optional) A relevant citation, e.g. "TCGA, Nature 2012".
#' @param pmid (Optional) One or more relevant pubmed ids (comma separated without whitespace); if used, citation cannot be `NULL`.
#' @param groups (Optional) Defaults to "PUBLIC" for use with public cBioPortal; 
#' otherwise, use group names that makes sense with the configuration of your cBioPortal instance.
#' @param add_global_case_list( (Optional) Use `NULL` to ignore, default is `TRUE` for an "All samples" case list( to be generated automatically.
#' @param verbose Verbosity level.
#' 
#' @export
cbp_new_study <- function(cancer_study_identifier,
                          name,
                          type_of_cancer,
                          description = "The data are contributed by researchers funded by the Neurofibromatosis Therapeutic Acceleration Program (NTAP). 
                          The reprocessing of the raw data is managed by the NF Open Science Initiative (https://nf.synapse.org/).",
                          short_name = NULL,
                          citation = NULL,
                          pmid = NULL,
                          groups = "PUBLIC",
                          add_global_case_list = TRUE,
                          verbose = TRUE) {
  
  # TODO Validate study id
  study_dir <- cancer_study_identifier
  if(!dir.exists(study_dir)) {
    if(verbose) message(glue::glue("✔ Creating {study_dir} study directory"))
    dir.create(glue::glue("./{study_dir}"))
  }
  
  message("✔ Setting dataset directory as working directory")
  setwd(study_dir)
  
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
  if(verbose) message("✔ Study meta added")
}

# ------------------------------------------------------------------------------- #


#' Export and add clinical data to cBioPortal dataset
#' 
#' This should be run in an existing dataset package root. 
#'
#' Clinical data are mapped and exported according to a reference mapping. 
#' Also reformatting of `PATIENT_ID`, `SAMPLE_ID` to contain only letters, numbers, points, underscores, hyphens;
#' in Nextflow processing any spaces gets replaced with underscores so that's the default here.
#' Does *not* check for missing samples, as final validation via cBioPortal tool is still expected for that.
#' 
#' @param ref_view A view that contains all clinical data for the study.
#' @param ref_map YAML file specifying the mapping of (NF) clinical metadata to cBioPortal model. See details.
#' @param verbose Whether to provide informative messages throughout.
cbp_add_clinical <- function(ref_view,
                             ref_map,
                             verbose = TRUE) {
  
  cancer_study_identifier <- check_cbp_study_id()
  
  if(verbose) message("✔ Pulling the clinical data from Synapse")
  df <- get_clinical_data_for_cbp_study(ref_view)
  
  if(verbose) message("✔ Formatting and making clinical data file(s)")
  df$specimenID <- gsub(" ", "_", clinical_data$specimenID)
  write_cbio_clinical(df, ref_map = ref_map, verbose = verbose)
  
  if(verbose) message("✔ Making sample clinical meta file")
  make_meta_sample(cancer_study_identifier, verbose = verbose)
  
  # Before making meta, check that the optional patient data file was written 
  if(file.exists("data_clinical_patient.txt")) {
    if(verbose) message("✔ Making patient clinical meta file")
    make_meta_patient(cancer_study_identifier, verbose = verbose)
  }
  
  if(verbose) message("✔ Done with adding clinical data")
  
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
#' @param maf_data Synapse id of `merged maf` file for public release.
#' @param samplesheet Synapse id or local path to samplesheet with release info.
#' @param cancer_study_identifier Study identifier, convention is `{tumorType}_{institution}_{year}`, so for example "mpnst_nfosi_2022".
#' @param verbose Whether to provide informative messages throughout.
#' @export
cbp_add_maf <- function(maf_data,
                        samplesheet,
                        verbose = TRUE) {
  
  .check_login()
  cancer_study_identifier <- check_cbp_study_id()
  
  if(verbose) message("✔ Getting `maf_data` file from Synapse")
  file <- .syn$get(maf_data, downloadLocation = ".")
  data_mutations <- sub(file$name, "data_mutations.txt", file$path)
  file.rename(file$path, data_mutations)
  
  if(verbose) message("✔ Checking the `maf` release file against samplesheet")
  mm <- dt_read(data_mutations_extended)
  ss <- dt_read(samplesheet)
  check_result <- check_maf_release(mm, ss)
  
  if(!is.null(check_result)) stop("Unfortunately, check of `maf` release failed so will not continue. Please update data and retry.")
  
  if(verbose) message("✔ Making maf meta file")
  make_meta_maf(cancer_study_identifier, verbose = verbose)
  
  if(verbose) message("✔ Done with adding MAF data")
  
}


#' Check maf file for release
#' 
#' Currently, this is a simple check to make sure released samples are expected. 
#' It may be extended later on as needed. 
#' 
#' @param maf_data Maf data as a `data.table`.
#' @param samplesheet Samplesheet as a `data.table`.
#' @return Returns `NULL` if everything OK, else the sample ids that don't match expectations.
check_maf_release <- function(maf_data, 
                              samplesheet) {
  
  # samplesheet[is_releasable == TRUE, .N]
  ss_samples <- samplesheet[is_releasable == TRUE, biospecimen_id]
  mm_samples <- maf_data[, unique(Tumor_Sample_Barcode)]
  
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
  if(!length(missing_release) && !length(no_release)) test_passed("✔ Samples for release look as expected.")
  
  invisible(result)
}


# ------------------------------------------------------------------------------- #

#' Export and add CNA (seg) data to cBioPortal dataset
#' 
#' This should be run in an existing dataset package root.
#' 
#' @export
#' 
#' @param cna_release Syn id of CNA release data, currently only handles `.seg` file.
cbp_add_cna <- function(cna_data) {
  
  cancer_study_identifier <- check_cbp_study_id()
  
  if(verbose) message("✔ Getting the CNA (.seg) data file from Synapse")
  file <- .syn$get(cna_data, downloadLocation = ".")
  data_cna <- sub(file$name, "data_cna.seg", file$path)
  file.rename(file$path, data_cna)
  
  if(verbose) message("✔ Making the meta file")
  make_meta_cna(...)
  
  if(verbose) message("✔ Done with adding CNA data")
  
} 


#' Export and add expression data to cBioPortal dataset
#' 
#' This should be run in an existing dataset package root.
#' 
#' @export
#' 
#' @param mrna_data Syn id of gene expression data.
cbp_add_mrna <- function(mrna_data) {
  
  cancer_study_identifier <- check_cbp_study_id()
  
  if(verbose) message("✔ Getting the mRNA expression data file from Synapse")
  file <- .syn$get(cna_release, downloadLocation = ".")
  data_cna <- sub(file$name, "data_cna.seg", file$path)
  file.rename(file$path, data_cna)
  
  if(verbose) message("✔ Making the meta file")
  make_meta_cna(cancer_study_identifier)
  
  if(verbose) message("✔ Done with adding CNA data")
  
} 

