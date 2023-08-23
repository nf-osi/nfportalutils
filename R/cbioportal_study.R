# Export data from Synapse as a cBioPortal dataset, where different data types can be added to the package one-by-one,
# much in the spirit of https://github.com/r-lib/usethis.
# This file contains the higher-level wrappers that are exported; see `cboilerplate.R` for the non-exported lower-level utils.
# All functions should start with `cbp_*` so that it's clear this is cBioPortal-relevant functionality.

#' Enumerate combinations of valid cBP data types and data subtypes and helper utils if available
#' 
#' https://docs.cbioportal.org/file-formats/
#' 
#' @keywords internal
cbp_datatypes <- function() {

  types <- data.table::rbindlist(list(
    list("CLINICAL", "SAMPLE_ATTRIBUTES", "cbp_add_clinical", ""),
    list("CLINICAL", "PATIENT_ATTRIBUTES", "cbp_add_clinical", ""),
    list("COPY_NUMBER_ALTERATION", "SEG", "cbp_add_cna", ""),
    list("COPY_NUMBER_ALTERATION", "DISCRETE", "", ""),
    list("COPY_NUMBER_ALTERATION", "DISCRETE_LONG", "", ""),
    list("COPY_NUMBER_ALTERATION", "CONTINUOUS", "", ""),
    list("COPY_NUMBER_ALTERATION", "LOG2-VALUE", "", ""),
    list("MRNA_EXPRESSION", "CONTINUOUS", "cbp_add_expression", ""),
    list("MRNA_EXPRESSION", "Z-SCORE", "cbp_add_expression", ""),
    list("MUTATION_EXTENDED", "MAF", "cbp_add_maf", ""),
    list("METHYLATION", "CONTINUOUS", "", ""),
    list("PROTEIN_LEVEL", "LOG2-VALUE", "", ""),
    list("STRUCTURAL_VARIANT", "SV", "", "")))
  
  setnames(types, c("dataType", "dataSubtype", "fun", "note"))
  return(types)
  }

#' Initialize a new cBioPortal study dataset
#' 
#' Create a new directory with a basic required [study meta file](https://docs.cbioportal.org/file-formats/#meta-file),
#' much like how we'd create a new R package and put a DESCRIPTION file in it.
#' 
#' @param cancer_study_identifier Cancer study identifier in format such as `nst_nfosi_ntap_2022`.
#' @param name Name of the study, e.g. "Malignant Peripheral Nerve Sheath Tumor (NF-OSI, 2022)".
#' @param type_of_cancer Id for type of cancer. If `validate` is TRUE, this is one of the things validated with warning if mismatched.
#' @param description Description of the study, defaults to a generic description that can be edited later.
#' @param short_name (Optional) Short name for the study.
#' @param citation (Optional) A relevant citation, e.g. "TCGA, Nature 2012".
#' @param pmid (Optional) One or more relevant pubmed ids (comma-separated, no whitespace); if used, citation cannot be `NULL`.
#' @param groups (Optional) Defaults to "PUBLIC" for use with public cBioPortal; 
#' otherwise, use group names that makes sense with the configuration of your cBioPortal instance.
#' @param add_global_case_list (Optional) Use `NULL` to ignore, default is `TRUE` for an "All samples" case list( to be generated automatically.
#' @param validate Validate against public cBioPortal configuration. Default `TRUE`, 
#' but might want to set to `FALSE` especially if using a custom cBioPortal instance with different configuration. 
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
                          validate = TRUE,
                          verbose = TRUE) {
  
  # TODO Validate study id
  study_dir <- cancer_study_identifier
  if(!dir.exists(study_dir)) {
    if(verbose) checked_message(glue::glue("Creating {study_dir} study directory"))
    dir.create(glue::glue("./{study_dir}"))
  }
  
  checked_message("Setting dataset directory as working directory")
  setwd(study_dir)
  
  if(validate) {
    tryCatch({
      cancer_ids <- httr::GET("https://www.cbioportal.org/api/cancer-types?direction=ASC&pageNumber=0&pageSize=10000&projection=SUMMARY") %>% 
        httr::content() %>%
        sapply(`[[`, "cancerTypeId")
      if(!type_of_cancer %in% cancer_ids) {
        warning("Specified `type_of_cancer id` isn't in the public list. 
                Check for typos or include additional meta for this cancer type -- see https://docs.cbioportal.org/file-formats/#cancer-type")
      }
    }, error = function(e) warning("Note: Other issue with validation via public API...skipping."))
  }
  
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
  if(verbose) checked_message("Study meta added")
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
  
  if(verbose) checked_message("Pulling the clinical data from Synapse")
  df <- get_clinical_data_for_cbp_study(ref_view)
  
  if(verbose) checked_message("Formatting and making clinical data file(s)")
  df$specimenID <- gsub(" ", "_", clinical_data$specimenID)
  write_cbio_clinical(df, ref_map = ref_map, verbose = verbose)
  
  if(verbose) checked_message("Making sample clinical meta file")
  make_meta_sample(cancer_study_identifier, verbose = verbose)
  
  # Before making meta, check that the optional patient data file was written 
  if(file.exists("data_clinical_patient.txt")) {
    if(verbose) checked_message("Making patient clinical meta file")
    make_meta_patient(cancer_study_identifier, verbose = verbose)
  }
  
  if(verbose) checked_message("Done with adding clinical data")
  
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
#' @param maf_data Synapse id of `merged maf` file for public release.
#' @param verbose Whether to provide informative messages throughout.
#' @export
cbp_add_maf <- function(maf_data,
                        verbose = TRUE) {
  
  .check_login()
  cancer_study_identifier <- check_cbp_study_id()
  
  if(verbose) checked_message("Getting `maf_data` file from Synapse")
  file <- .syn$get(maf_data, downloadLocation = ".")
  data_mutations <- sub(file$name, "data_mutations.txt", file$path)
  file.rename(file$path, data_mutations)
  
  if(verbose) checked_message("Making maf meta file")
  make_meta_maf(cancer_study_identifier, verbose = verbose)
  
  if(verbose) checked_message("Done with adding MAF data")
  
}


# ------------------------------------------------------------------------------- #

#' Export and add CNA (seg) data to cBioPortal dataset
#' 
#' This should be run in an existing dataset package root.
#' 
#' @param cna_data Synapse id of CNA data file, currently only handles `.seg` file.
#' @export
cbp_add_cna <- function(cna_data) {
  
  cancer_study_identifier <- check_cbp_study_id()
  
  if(verbose) checked_message("Getting the CNA (.seg) data file from Synapse")
  file <- .syn$get(cna_data, downloadLocation = ".")
  data_cna <- sub(file$name, "data_cna.seg", file$path)
  file.rename(file$path, data_cna)
  
  if(verbose) checked_message("Making the meta file")
  make_meta_cna(cancer_study_identifier)
  
  if(verbose) checked_message("Done with adding CNA data")
  
} 


#' Export and add expression data to cBioPortal dataset
#' 
#' This should be run in an existing dataset package root.
#' 
#' @export
#' 
#' @param expression_data Syn id of gene expression data.
cbp_add_expression <- function(expression_data) {
  
  cancer_study_identifier <- check_cbp_study_id()
  
  if(verbose) checked_message("Getting the mRNA expression data file from Synapse")
  file <- .syn$get(expression_data, downloadLocation = ".")
  data_expression <- sub(file$name, "data_expression.txt", file$path)
  file.rename(file$path, data_expression)
  
  if(verbose) checked_message("Making the meta file")
  make_meta_expression(cancer_study_identifier)
  
  if(verbose) checked_message("Done with adding expression data")
  
} 

