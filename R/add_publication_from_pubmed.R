#' Add a publication to the publication table
#' 
#' Requires that the publication be in PubMed to derive metadata such as authors, title, etc.
#' Fields `disease_focus` and `manifestation` can be supplemented manually by the curator. 
#' The `study_id` is used to join with study table to create view with consistent `studyName` and `fundingAgency`. 
#' 
#' @param publication_table_id The synapse id of the portal publication table. Must have write access.
#' @param pmid The PubMed ID (*not* PMCID) of the publication to be added.
#' @param study_id The synapse id(s) of the study that are associated with the publication. 
#' @param disease_focus The disease focus(s) that are associated with the publication.
#' @param manifestation The manifestation(s) that are associated with the publication.
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
#'                study_table_id = "",
#'                dry_run = T)
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
#' Get PMID meta as table with `title`, `journal`, `author`, `year`, `pmid`, `doi` and, 
#' optionally, `keywords`. If PMID is not found, returns `NA`.
#' 
#' @param pmid PubMed id number.
#' @param with_keywords Whether to include keywords.
#' @export
from_pubmed <- function(pmid, with_keywords = TRUE) {
  
  res <- easyPubMed::get_pubmed_ids(pmid) 
  if(res$Count == 0) { 
    message("Nothing found for PMID")
    return(NA) # Return NA early if no records found 
  }
  res <- easyPubMed::fetch_pubmed_data(pmids, format = "xml", retmax = 1)
  
  p <- xml2::as_list(xml2::read_xml(res[[1]]))
  
  authors <- mapply(function(author) paste(author$ForeName, author$LastName), 
                    p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$AuthorList) 
  journal <- tools::toTitleCase(p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Journal$Tile[[1]])
  title <- p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ArticleTitle[[1]]
  doi <- paste0("https://www.doi.org", p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ELocationID[[1]])
  year <- p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year[[1]]

  record <- data.frame(title = title, journal = journal, author = I(list(authors)),
                       year = year, pmid = pmid, doi = doi)
  
  if(with_keywords) {
    record$keywords <- list(unique(unlist(p$PubmedArticleSet$PubmedArticle$MedlineCitation$MeshHeadingList, 
                  p$PubmedArticleSet$PubmedArticle$MedlineCitation$KeywordList)))
  }
  return(record)
}

