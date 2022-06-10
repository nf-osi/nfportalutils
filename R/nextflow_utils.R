#' Parse a nextflow samplesheet to construct input-output map
#' 
#' NF samplesheets are used in rnaseq pipelines,
#' and are defined here: https://nf-co.re/rnaseq/usage#full-samplesheet.
#' Currently this is the most obvious way to get refs for provenance annotation.
#' 
#' @param samplesheet A local file or syn id of samplesheet.
#' @export
mapSampleInput <- function(samplesheet) {
  
  if(file.exists(samplesheet)) {
    path <- samplesheet
  } else if(grepl("^syn", samplesheet)) {
    message("Getting samplesheet from Synapse...")
    .check_login()
    path <- .syn$get(samplesheet)$path
  } else {
    stop("Samplesheet must be local file or accessible synapse file.")
  }
  
  ss <- data.table::fread(path)
  # Get synId from URI
  ss[, input_syn_1 := bareSynId(fastq_1)]
  ss[, input_syn_2 := bareSynId(fastq_2)] 
  
  # Parse sample from "sample"
  ss[, sample_parsed := gsub("_T[0-9]$", "", sample)]
  
  # Create ref map with file inputs for each sample specimen
  sample_input_map <- ss[, .(inputs = c(input_syn_1, input_syn_2)), by = sample_parsed]
  sample_input_map <- split(sample_input_map, by = "sample_parsed", keep.by = FALSE)
  sample_input_map <- lapply(sample_input_map, unlist, use.names = FALSE)
  return(sample_input_map)
}

#' Create local view of some scopes
#' 
#' Creating a fileview or dataset can be useful for filtering/selection,
#' but sometimes one wants to avoid having to do that directly on synapse,
#' because queries and query result downloads can be slower and not as facile.
#' As well, it can be somewhat messy to have many helper fileviews in a project.
#' This creates a local view with default props that works well enough for certain jobs.
#' 
#' @param scope Vector of one or more syn container ids.
#' @export
localView <- function(scope, idcol = "parent", idmap = NULL) {
  result <- list()
  for(i in scope) {
    children <- .syn$getChildren(parent = i)
    meta <- iterate(children)
    result[[i]] <- data.table::rbindlist(meta)
  }
  if(!is.null(idmap)) names(result) <- idmap[names(result)]
  localview <- data.table::rbindlist(result, idcol = idcol)
  return(localview)
}

#' Map out processed RNA-seq files in nextflow output repo 
#' 
#' In specified location where workflow has deposited the outputs,
#' map out the processed files based on relevant file extensions and 
#' tie these files to source samples using preliminary strategies
#' that rely on file location and file naming.
#' Since Nextflow (and other workflow tools) often nests outputs by sample-id folders, 
#' this looks for processed files within one level of nesting.
#' 
#' @param folder Syn id of folder with files of interest. 
#' @param file_ext Regex of relevant file extensions; defaults to processed RNA-seq formats.
#' @export
mapProcessedRNASeq <- function(folder = "syn30840584", 
                                 file_ext = ".bam$|.bai$|.sf$") {
                                   
  outputs <- localView(folder)
  processed <- outputs[grepl(file_ext, name), .(name, id, parent)]
  folders <- outputs[type == "org.sagebionetworks.repo.model.Folder", .(name, id)]
  nested_outputs <- localView(folders$id, idmap = setNames(folders$name, folders$id))
  nested_processed <- nested_outputs[grepl(file_ext, name), .(name, id, parent)]
  all <- rbind(processed, nested_processed)
  all[, file_ext := tools::file_ext(name)]
  all[file_ext == "sf", sample :=  parent]
  all[file_ext %in% c("bam", "bai"), sample :=  gsub("[.].*", "", name)]
  
  
  sample_l2_output_map <- split(all[file_ext %in% c("bam", "bai"), .(id, sample)], by = "sample", keep.by = F) 
  sample_l3_output_map <- split(all[file_ext %in% "sf", .(id, sample)], by = "sample", keep.by = F) 
  sample_output_map <- lapply(sample_output_map, unlist, use.names = FALSE)
  return(sample_output_map)
} 

#' Add provenance for nextflow RNA-seq results
#' 
#' Composes together some utils to do provenance annotation in one step 
#'
#' @inheritParams mapSampleInput
#' @param workflow Workflow name (activity name).
#' @param workflow_link Workflow link.
#' @export
addProvRNASeq <- function(samplesheet, 
                          workflow = "STAR and Salmon",
                          workflow_link = "https://nf-co.re/rnaseq/3.4/output#star-and-salmon") {
  
  sample_inputs <- mapSampleInput(samplesheet)
  # Translate sample names to syn file ids
  
  
  addActivityBatch(names(sample_inputs), 
                   workflow, 
                   workflow_link,
                   sample_inputs
  )
}

#' Extract synapse id from URI or other string
#' 
#' @param uri
#' @keywords internal
bareSynId <- function(uri) regmatches(uri, regexpr("syn[0-9]{8}+", uri))

