#' Make cBioPortal mutations dataset from Synapse assets
#' 
#' The NF-OSI workflow produces a single merged maf file that represents a filtered subset of the `maf`s,
#' including only the (non-germline) data that _can_ be released for cBioPortal.
#' However, this data file by itself is not immediately loadable into an instance of a cBioPortal server.
#' It needs to be packaged with some other files, 
#' such as this [example of a minimal public dataset with mutations data](https://github.com/cBioPortal/datahub/tree/1e03ea6ab5e0ddd497ecf349cbee7d50aeebcd5e/public/msk_ch_2020). 
#' To create this bundle of cBioPortal files, this wrapper calls a number of lower-level functions to do the following:
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
#' In the future, it would be preferable to store clinical metadata in a separate normalized table.
#' For now, this clinical data is pulled in from the view. To map NF clinical variables to the 
#' [cBioPortal dictionary](https://github.com/cBioPortal/clinical-data-dictionary/blob/e9ec08f48bd57aabf193da70cdb5b88bdef5d01d/docs/resource_uri_to_clinical_attribute_mapping.txt)
#' [as recommended](https://docs.cbioportal.org/file-formats/#custom-columns-in-clinical-data), 
#' this step requires a `ref_map`, which is a YAML file.
#' 
#' 3. Make meta files. Meta files are needed for describing the study, mutations data file, and clinical data files. 
#' 
#' This wrapper creates the dataset, but there are additional steps such as validation that have to be done outside of R (with a cBioPortal instance).
#' You do not need necessarily need to set up a full local development server but will need the cBioPortal back-end at least.
#' See [docs for the dataset validation](https://docs.cbioportal.org/using-the-dataset-validator/).
#' You can use the back-end image such as `cbioportal/cbioportal:4.1.13` and mount the dataset into the container 
#' for running validation in offline mode, e.g. with the command 
#' `docker run -e S=/cbioportal/nfosi_2022 -v $(pwd)/nfosi_2022:/cbioportal/nfosi_2022 -w /cbioportal/core/src/main/scripts/importer --entrypoint /bin/bash cbioportal/cbioportal:4.1.13 -c './validateData.py -s $S -p ../../../test/scripts/test_data/api_json_system_tests/ -v'`
#' 
#' @param merged_maf Synapse id or local path to merged maf file for public release, to be checked with samplesheet.
#' @param samplesheet Synapse id or local path to samplesheet with release info.
#' @param ref_view A view or table that contains clinical data for the samples.
#' @param ref_map YAML file specifying the mapping of (NF) clinical metadata to cBioPortal model. 
#' This follows a specific format.
#' @param publish_dir Where to output the cBioPortal set of files. 
#' Defaults to a folder called "nfosi_{current year}" in the current working directory.
#' @export
syncBP_maf <- function(merged_maf,
                       samplesheet,
                       ref_view, 
                       ref_map,
                       publish_dir = glue::glue("nfosi_{format(Sys.Date(), '%Y')}")) {
  
  message("--- Checking the `merged_maf` files ---")
  mm <- dt_read(merged_maf) # don't bother with maftools dependency as doing basic check
  ss <- dt_read(samplesheet)
  check_result <- check_maf_release(mm, ss)
  
  if(!is.null(check_result)) stop("Unfortunately, check failed so will not continue.")
  if(!dir.exists(publish_dir)) dir.create(publish_dir)
  
  message("--- Making the clinical data files ---")
  
  # TO DO
  
  message("--- Making the meta files ---")
  
}


#' Check maf file for release
#' 
#' Currently, this is a simple check to make sure released samples are expected. 
#' It may be extended later on as needed. 
#' 
#' @param merged_maf Maf file as a `data.table`.
#' @param samplesheet Samplesheet as a `data.table`.
#' @return Returns `NULL` if everything OK, else the sample ids that don't match expectations.
check_maf_release <- function(merged_maf, samplesheet) {
  
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
