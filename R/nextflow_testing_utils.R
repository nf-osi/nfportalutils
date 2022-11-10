#' Check fastq read pair matches samplesheet read pair assignment.
#'
#' Read pairs are often encoded in the name of the file. Here, we check that if encoded in the name of the file,
#' that the samplesheet read pair (e.g. _1 or _2) matches
#'
#' @param samplesheet A local file or syn id of samplesheet.
#' @param parse_fun Function implementing how to parse samples in samplesheet.
#' @import data.table
#' @examples
#' \dontrun{
#'  check_readpair_validity('syn39542932')
#'  check_readpair_validity('syn29530880')
#' }
#' @export

check_readpair_validity <- function(samplesheet,
                                    parse_fun = function(x) gsub("_T[0-9]$", "", x)) {

  samples <- dt_read(samplesheet)
  samples[, "1" := bare_syn_id(fastq_1)] # Get synId from URI
  samples[, "2" := bare_syn_id(fastq_2)]
  samples[, sample := parse_fun(sample)]
  samples <- samples[, c("sample", "1", "2")]

  samples %>% tidyr::pivot_longer(cols = c("1", "2"), values_to = "input_id", names_to = "readPair") %>%

    dplyr::mutate(named_rp = sapply(input_id, function(i){
      string <- .syn$get(entity = i, downloadFile = F)$name
      rp <- identify_read_pair(string)
    })) %>%
    dplyr::mutate(readpair_matches = dplyr::case_when( ##if this function runs slow on large datasets this could be a place to optimize
      named_rp == readPair ~ TRUE,
      !is.null(named_rp) & named_rp != readPair ~ FALSE))

  if(!all(samples$readpair_matches)){
    test_failed("Samplesheet read pair mismatch detected, check output.")
  }else{
    test_passed("All samplesheet read pairs match expected values.")
  }

  samples

}

#' Identify read pair from string
#'
#' @param string A filename string.
#' @return Returns a read pair: 1, 2, or NULL if none detected.
#'
#'
identify_read_pair <- function(string){

  read_pair <- if(stringr::str_detect(string,"[Rrl\\.\\_]1.fastq")){
      #matching "R1.fastq" or "r1.fastq"  or "l1.fastq" or ".1.fastq"  or " _1.fastq" style
      ##"l1.fastq" seems to come from one lab in particular and may not be relevant for processed data
    1
    }else if(stringr::str_detect(string,"[Rrl\\.\\_]2.fastq")){
    2
    }else if(stringr::str_detect(string,"[Rl]1_001.fastq")){ #matching R1_001.fastq or l1_001.fastq style
    1
    }else if(stringr::str_detect(string,"[Rl]2_001.fastq")){
    2
    }else if(stringr::str_detect(string,"s\\d_1_GSLv3-")){ #matching, e.g. "...s8_2_GSLv3-..." style
    1
    }else if(stringr::str_detect(string,"s\\d_2_GSLv3-")){
    2
    }

  read_pair
}

#' Check output strandedness matches samplesheet strandedness
#'
#' This function allows you to (post-workflow) check that the library type inferred by
#' during processing matches the "strandedness" provided for each sample in the workflow sample sheet.
#'
#' @param multiqc_id The Synapse ID of the "multiqc_data.json" output file
#' @examples
#' \dontrun{
#'  out <- check_libtype_validity(multiqc_id = 'syn29530566') #should pass
#'  out <- check_libtype_validity(multiqc_id = 'syn34573377') #should fail
#' }
#' @export
check_libtype_validity <- function(multiqc_id){

  mqc <- extract_libtype(multiqc_id)

  if(length(mqc)==1 && is.na(mqc)){
    test_passed("All samplesheet library type/strandedness match expected values.")
  }else{
    mqc <- mqc %>%
      dplyr::mutate(libtype_matches = inferred_strandedness == provided_strandedness)

    fail_length <- length(mqc$libtype_matches)-sum(mqc$libtype_matches)

    test_failed(glue::glue("Samplesheet library type/strandedness mismatch detected in {fail_length} samples, check output."))

    mqc
  }
}


#' Extract the library type from multiqc_data.json in nf-core/rnaseq output
#'
#' @param syn_id A synapse id for the multiqc_data.json file.
#' @returns a data frame of library types
#' @import jsonlite
#' @import jqr
#'
extract_libtype <- function(multiqc_id){
  jsn <- .syn$get(multiqc_id)$path
  foo <- jsonlite::fromJSON(jqr::jq(file(jsn), '[. | {warn: .report_saved_raw_data."multiqc_warning:_fail_strand_check"}]'), flatten = F )
  if(length(foo$warn)==1 && is.na(foo$warn)){
    NA
  }else{
  lapply(foo$warn, function(x){
    b <- x[['Inferred strandedness']]
    c <- x[['Provided strandedness']]
    c("inferred_strandedness" = b, "provided_strandedness" = c)
  }) %>%
    purrr::set_names(gsub("sample.", "",names(foo$warn))) %>%
    dplyr::bind_rows(.id = "sample")
  }
}


#' Format a test fail message.
#'
#' @param display_string A character string to format as a test failed message.
#' @returns A message to the console
#' @import emoji
#' @import crayon
#'
test_failed <- function(display_string){
  message(glue::glue("{emoji::emoji('broken_heart')} {crayon::bold(crayon::red('Test failed:'))} {crayon::red(display_string)}"))
}


#' Format a test passed message.
#'
#' @param display_string A character string to format as a test passed message.
#' @returns A message to the console
#' @import emoji
#' @import crayon
#'
test_passed <- function(display_string){
  message(glue::glue("{emoji::emoji('green_heart')} {crayon::bold(crayon::green('Test passed:'))} {crayon::green(display_string)}"))
}

