#' Add a publication to the publication table
#' 
#' Requires that the publication be in PubMed to auto-derive metadata such as authors, title, etc.
#' In contrast, `disease_focus` and `manifestation` need to be supplemented by the curator. 
#' The `study_id` is used to get consistent `studyName` and `fundingAgency` from study table without manual input. 
#' 
#' @param pmid PubMed ID (*not* PMCID) of the publication to be added.
#' @param study_id Synapse id(s) of the study that are associated with the publication. 
#' @param disease_focus The disease focus(s) that are associated with the publication.
#' @param manifestation The manifestation(s) that are associated with the publication.
#' @param publication_table_id Synapse id of the portal publication table. Must have write access.
#' @param study_table_id Synapse id of the portal study table. Need read access.
#' @param dry_run Default = TRUE. Skips upload to table and instead prints formatted publication metadata.
#' @return If dry_run == T, returns publication metadata to be added.
#' @examples 
#' \dontrun{
#' add_publication_from_pubmed(
#'                pmid = "33574490",
#'                study_id = "syn2343195",
#'                disease_focus = c("Neurofibromatosis"),
#'                manifestation = c("Meningioma"),
#'                publication_table_id = "syn16857542",
#'                study_table_id = "")
#'}
#' @export
add_publication_from_pubmed <- .add_publication_from_pubmed()

#' Higher-level fun to generate `add_publication_from_pubmed` util for one-off usage (default) or optimized for batch processing.
#' @param batch If a non-zero batch size, turns on batch mode; defaults to no-batch.
#' @param cache Whether to cache some results, which is default if `batch`.
#' @keywords internal
.add_publication_from_pubmed <- function(batch = 0L, cache = batch) {
  pmids <- NULL
  new_data <- NULL
  function(pmid, study_id, disease_focus, manifestation, 
           publication_table_id, study_table_id, dry_run = T) {
    
    .check_login()
    
    # Query only for data needed, i.e. PMID to check non-dup; result can be cached
    if(is.null(pmids)) {
      pmids <- table_query(publication_table_id, "pmid") %>% unlist(use.names = F)
      if(cache) pmids <<- pmids
    }
    
    if(pmid %in% pmids) {
      message(glue::glue("PMID:{pmid} already exists in destination table!")) 
    } else {
      record <- from_pubmed(pmid, nf_keywords = TRUE) 
      if(is.na(record)) return()
      
      study_id_set <- glue::glue_collapse(glue::single_quote(study_id), sep = ", ")
      study <- .syn$tableQuery(glue::glue("SELECT studyId, studyName, fundingAgency FROM {study_table_id} WHERE studyId IN ({study_id_query})"))$asDataFrame()
      record <- cbind(record, studyId = I(list(study$studyId)), studyName = I(list(study$studyName)), fundingAgency = I(list(study$fundingAgency)))
      
      # If batch mode, rbind and defer table schemafication until all records processed
      if(batch) {
        new_data <<- rbind(new_data, record)
        if(nrow(new_data) == batch) new_data <- as_table_schema(new_data, publication_table_id)
      } else {
        new_data <- as_table_schema(record, publication_table_id)
      }
      if(!dry_run) {
        new_data <- .syn$store(new_data)
        message(glue::glue('PMID:{new_data$pmid} added!'))
      } else{
        new_data
      }
    }
  }
}

#' Get publication metadata from PubMed
#' 
#' If PMID found, return meta as table w/ `title` `journal` `author` `year` `pmid` `doi`, else `NA`.
#' 
#' @param pmid PubMed id.
#' @export
from_pubmed <- function(pmid) {
  
  res <- easyPubMed::get_pubmed_ids(pmid) 
  if(res$Count == 0) { 
    message(glue::glue("Nothing found for PMID:{pmid}"))
    return(NA) # Return NA early if no records found 
  }
  p <- easyPubMed::fetch_pubmed_data(res, format = "xml", retmax = 1) %>% 
    `[[`(1) %>% xml2::read_xml() %>% xml2::as_list()
  
  authors <- mapply(function(author) paste(author$ForeName, author$LastName), 
                    p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$AuthorList) 
  journal <- tools::toTitleCase(p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Journal$Title[[1]])
  title <- p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ArticleTitle[[1]]
  doi <- paste0("https://www.doi.org", p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ELocationID[[1]])
  year <- p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year[[1]]
  # keywords <- unique(unlist(p$PubmedArticleSet$PubmedArticle$MedlineCitation$MeshHeadingList, 
  #                                p$PubmedArticleSet$PubmedArticle$MedlineCitation$KeywordList))
  
  record <- data.frame(title = title, journal = journal, author = I(list(authors)),
                       year = year, pmid = pmid, doi = doi)
  return(record)
}

