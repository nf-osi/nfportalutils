#' Add a publication to the publication table
#' 
#' Requires that the publication be in PubMed to derive metadata such as authors, title, etc.
#' Fields `disease_focus` and `manifestation` also derived but can be _supplemented_ by the curator as args. 
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
add_publication_from_pubmed <- function(pmid, 
                                        study_id, 
                                        disease_focus, 
                                        manifestation, 
                                        publication_table_id,
                                        study_table_id,
                                        dry_run = T) {
  .check_login()

  # Query only for data needed, i.e. PMID to check non-dup; don't need all data to add new row
  pmids <- table_query(publication_table_id, "pmid") %>% unlist(use.names = F)
  
  if(pmid %in% pmids) {
      message(glue::glue("PMID:{pmid} already exists in destination table!")) 
  } else {
    record <- from_pubmed(pmid) 
    if(is.na(record)) return()
    
    new_row <- table_join(record, study_table_id, studyId)  
    new_row <- as_table_schema(new_row, table = publication_table_id)
    if(!dry_run) {
      stored <- .syn$store(new_row)
      message(glue::glue('PMID:{pmid} added!'))
    } else{
      new_row
    }
  }
}


#' Get publication metadata from PubMed
#' 
#' If PMID found, return meta as table w/ `title` `journal` `author` `year` `pmid` `doi` `keywords`, else `NA`.
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
  journal <- tools::toTitleCase(p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Journal$Tile[[1]])
  title <- p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ArticleTitle[[1]]
  doi <- paste0("https://www.doi.org", p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ELocationID[[1]])
  year <- p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year[[1]]
  keywords <- unique(unlist(p$PubmedArticleSet$PubmedArticle$MedlineCitation$MeshHeadingList, 
                                 p$PubmedArticleSet$PubmedArticle$MedlineCitation$KeywordList))
  
  record <- data.frame(title = title, journal = journal, author = I(list(authors)),
                       year = year, pmid = pmid, doi = doi, keywords = I(list(keywords)))
  return(record)
}

