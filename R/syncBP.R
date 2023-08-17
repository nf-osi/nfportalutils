#' TO DO enhancements
#' 
#' When a dataset has already been created, one might want to only add or update a 
#' data type (bc clinical data is the same). Having an `add_*` or `update_*` 
#' type util might be useful, e.g. to add expression data to mutations data.


#' Make cBioPortal mutations dataset from Synapse assets
#' 
#' The NF-OSI workflow produces a single merged maf file that represents a filtered subset of the `maf`s,
#' containing only the (non-germline) data that _can_ be released for cBioPortal.
#' However, this data file by itself is not immediately loadable into an instance of a cBioPortal server
#' and needs to be packaged with other files, such as this 
#' [example of a public mutations dataset](https://github.com/cBioPortal/datahub/tree/1e03ea6ab5e0ddd497ecf349cbee7d50aeebcd5e/public/msk_ch_2020). 
#' This is a wrapper that goes through several steps needed to create said bundle of cBioPortal files more conveniently.
#' 
#' 1. A simple sanity check that this is the version of the maf release file that we want, based on the samplesheet.
#' For example, version 1 of the samplesheet will generate a version 1 of the merged maf, 
#' but if there is a later correction that retracts a sample (`is_releasable`=FALSE), 
#' that step of the workflow to generate merged maf will be rerun (or it should be), so we'll want to make sure the latest versions of these files are used.  
#' The latest versions of the samplesheets tied to each release are currently stored in `syn38793855`. 
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
#' @inheritParams make_meta_study
#' @param merged_maf Synapse id of `merged maf` file for public release.
#' @param samplesheet Synapse id or local path to samplesheet with release info.
#' @param ref_map YAML file specifying the mapping of (NF) clinical metadata to cBioPortal model. See details.
#' @param ref_view A view that contains clinical data for the release files.
#' @param name Name of the cancer study, e.g. something following convention is "Malignant Peripheral Nerve Sheath Tumor (NF-OSI, 2022)".
#' @param cancer_study_identifier Study identifier, convention is `{tumorType}_{institution}_{year}`, so for example "mpnst_nfosi_2022".
#' @param publish_dir Where to output the set of files. 
#' Defaults to (creating if necessary) a folder with same name as `cancer_study_identifier`.
#' @param verbose Whether to provide informative messages throughout.
#' @export
syncBP_maf <- function(merged_maf,
                       samplesheet,
                       ref_map,
                       ref_view, 
                       name,
                       cancer_study_identifier,
                       citation = NULL,
                       pmid = NULL,
                       short_name = NULL,
                       publish_dir = cancer_study_identifier,
                       verbose = TRUE) {
  
  .check_login()
  
  if(!dir.exists(publish_dir)) {
    if(verbose) message(glue::glue("Creating {publish_dir} dataset directory"))
    dir.create(publish_dir)
  }
  
  if(verbose) message("--- Getting `merged_maf` file ---")
  file <- .syn$get(merged_maf, downloadLocation = publish_dir)
  
  if(verbose) message("--- Standardizing `maf` release filename ---")
  data_mutations_extended <- sub(file$name, "data_mutations_extended.txt", file$path)
  file.rename(file$path, data_mutations_extended)
  
  if(verbose) message("--- Checking the `maf` release file against samplesheet ---")
  mm <- dt_read(data_mutations_extended) # don't bother with maftools dependency as doing basic check
  ss <- dt_read(samplesheet)
  check_result <- check_maf_release(mm, ss)
  
  if(!is.null(check_result)) stop("Unfortunately, check of `maf` release failed so will not continue.")
  
  if(verbose) message("--- Pulling the clinical data ---")
  df <- get_data_for_releasable(ss, ref_view, verbose = verbose)
  
  if(verbose) message("--- Matching clinical data sample ids to `maf` ---")
  df <- match_maf_sample_id(df)
  
  if(verbose) message("--- Making the clinical data files ---")
  write_cbio_clinical(df, ref_map = ref_map, publish_dir = publish_dir, verbose = verbose)
  
  if(verbose) message("--- Making the meta files ---")
  # only make meta patient if see that it has been outputted; 
  # meta sample is required and previous step will error otherwise
  if(file.exists(glue::glue("{publish_dir}/data_clinical_patient.txt"))) {
    make_meta_patient(cancer_study_identifier, publish_dir = publish_dir, verbose = verbose)
  }
  make_meta_sample(cancer_study_identifier, publish_dir = publish_dir, verbose = verbose)
  make_meta_maf(cancer_study_identifier, publish_dir = publish_dir, verbose = verbose)
  
  # If a single value in tumorType, use that, otherwise "mixed" as the catch-all
  type_of_cancer <- unique(df$tumorType)
  type_of_cancer <- type_of_cancer[type_of_cancer != ""]
  if(length(type_of_cancer) != 1) {
    type_of_cancer <- "mixed" 
    if(verbose) message("More than one cancer type detected in data, using `mixed` for study.")
  } 
 
  # Uses defaults for `groups` and `global_case_list`
  make_meta_study(cancer_study_identifier = cancer_study_identifier,
                  type_of_cancer = type_of_cancer,
                  name = name,
                  description = "The mutation data were processed using the `sarek` nf-core pipeline. The data are contributed by researchers funded by the Neurofibromatosis Therapeutic Acceleration Program (NTAP). The reprocessing of the raw data is managed by the NF Open Science Initiative (https://nf.synapse.org/).", 
                  citation = citation,
                  pmid = pmid,
                  short_name = short_name,
                  publish_dir = publish_dir, 
                  verbose = verbose)
  
  if(verbose) message("All files have been added successfully.")
  
}


#' Download data from a view for releasable samples in samplesheet 
#' 
#' This tries to check that complete data could be retrieved from said view. 
#' Note: Since the view is typically denormalized, not all data might be clinical. 
#' A downstream step will do some of the additional processing/subsetting needed.
#'  
#' @param samplesheet Samplesheet `data.table`.
#' @param ref_view View to get data from. 
#' @param verbose Output details.
#' @keywords internal
get_data_for_releasable <- function(samplesheet, 
                                    ref_view, 
                                    verbose = TRUE) {
  
  # bc specimen ids in samplesheet are different than actual ids on the files to avoid spaces, use file ids
  # ids <- samplesheet[is_releasable == TRUE, biospecimen_id]
  ss_key <- "synapse_id"
  rv_key <- "id"
  ids <- samplesheet[is_releasable == TRUE, get(ss_key)]
  ls <- glue::glue_collapse(glue::single_quote(ids), sep = ",")
  n <- length(ids)
  if(verbose) message(glue::glue("Retrieving from {ref_view} data for {n} releasable ids"))
  ref_view <- .syn$tableQuery(glue::glue("SELECT * FROM {ref_view} WHERE {rv_key} in ({ls})"))
  df <- ref_view$asDataFrame()
  if(nrow(df) != n) stop(glue::glue("Data retrieved for {nrow(df)} of {n} release ids. Is this right `ref_view`?"))
  return(df)
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


