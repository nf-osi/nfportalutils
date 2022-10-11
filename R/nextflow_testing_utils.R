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
  samples <- map_sample_input_ss(samplesheet, parse_fun) %>%
    tidyr::unnest_longer("input_id", indices_to = "readPair") %>%
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
#' Salmon (ISR, ISF, IU) during processing matches the "strandedness" provided for each sample in the workflow sample sheet.
#'
#' @param syn_out The Synapse ID of the "star_salmon" output folder
#' @param samplesheet A local file or syn id of samplesheet.
#' @param parse_fun Function implementing how to parse samples in samplesheet.
#' @import data.table
#' @examples
#' \dontrun{
#'  check_libtype_validity(syn_out = 'syn30840584', samplesheet = "syn30841441") #should pass
#'  check_libtype_validity(syn_out = 'syn30840584', samplesheet = "syn39593587") #should fail
#' }
#' @export
check_libtype_validity <- function(syn_out,
                                   samplesheet,
                                   parse_fun = function(x) gsub("_T[0-9]$", "", x)){

  samples <- dt_read(samplesheet)
  samples[, sample := parse_fun(sample)]
  samples <- samples[, c("sample", "strandedness")]

  message("Going through the outputs...this may take some seconds.")
  .o <- walk(syn_out)

  cmd_info <- lapply(.o[2:length(.o)], function(x) {
    if("cmd_info.json" %in% unlist(x)) list(sample = x[[1]][[1]],
                                       output_id = c(x[[3]][[1]][[2]]))
  }) %>%
    data.table::rbindlist() %>%
    dplyr::mutate(sample = gsub("^.*/", "", sample)) %>%
    dplyr::mutate(salmon_libtype = sapply(output_id, extract_libtype)) %>%
    dplyr::mutate(salmon_libtype_mod = dplyr::case_when(
      salmon_libtype=="IU" ~ "unstranded",
      salmon_libtype=="ISF" ~ "forward",
      salmon_libtype=="ISR" ~ "reverse",
    )) %>%
    dplyr::full_join(samples) %>%
    dplyr::mutate(libtype_matches = salmon_libtype_mod == strandedness)

  if(!all(cmd_info$libtype_matches)){
    test_failed("Samplesheet library type/strandedness mismatch detected, check output.")
  }else{
    test_passed("All samplesheet library type/strandedness match expected values.")
  }

  cmd_info
}


#' Extract the library type from cmd_info.json in nf-core/rnaseq output
#'
#' @param syn_id A synapse id for cmd_info.json
#' @returns a string: ISR, ISF, or IU
#' @import jsonlite
#'
extract_libtype <- function(syn_id){
  jsn <- .syn$get(syn_id)$path
  foo <- jsonlite::read_json(jsn)
  foo$libType
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

