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
