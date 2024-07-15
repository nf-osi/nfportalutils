#' Higher-level fun to generate `add_publication_from_pubmed` util for one-off usage (default) or optimized for batch processing.
#' @param batch If a non-zero batch size, turns on batch mode; defaults to no-batch.
#' @param cache Whether to cache some results, which is default if `batch`.
#' @keywords internal
.add_publication_from_pubmed <- function(batch = 0L, cache = batch) { # implement logging for batch?
  pmids <- new_data <- NULL
  counter <- 0L
  function(pmid, study_id,
           disease_focus = c(""), manifestation = c(""),
           publication_table_id, study_table_id, dry_run = T) {

    counter <<- counter + 1L
    # cat("current record:", counter) # make verbose?
    # Query only for data needed, i.e. PMID to check non-dup; result can be cached
    if(is.null(pmids)) {
      pmids <- synapser::synTableQuery(glue::glue("select pmid from {publication_table_id}")) %>%
        synapser::as.data.frame() %>%
        unlist(use.names = F)
      pmids <- gsub("PMID:", "", pmids)
      if(cache) pmids <<- pmids
    }

    if(pmid %in% pmids) {
      message(glue::glue("PMID:{pmid} already exists in destination table!")) # Possible that PMID needs to link to other study IDs
    } else {
      record <- from_pubmed(pmid)
      if(!length(record)) return()

      study_id_set <- glue::glue_collapse(glue::single_quote(study_id), sep = ", ")
      study <- synapser::synTableQuery(glue::glue("SELECT studyId, studyName, fundingAgency FROM {study_table_id} WHERE studyId IN ({study_id_set})"), includeRowIdAndRowVersion = F)%>%
        synapser::as.data.frame()
      record <- cbind(record,
                      diseaseFocus = I(list(disease_focus)),
                      manifestation = I(list(manifestation)),
                      studyId = I(list(study$studyId)),
                      studyName = I(list(study$studyName)),
                      fundingAgency = I(unique(sapply(study$fundingAgency, jsonlite::fromJSON))))

      # If batch mode, rbind and defer table schemafication until all records processed
      if(batch) {
        new_data <<- rbind(new_data, record)
        if(counter == batch) new_data <- as_table_schema(new_data, publication_table_id) else return()
      } else {
        new_data <- as_table_schema(record, publication_table_id)
      }
      if(!dry_run) {
        new_data <- synapser::synStore(new_data)
        message(glue::glue('Added new pmid(s)!'))
      } else {
        new_data
      }
    }
  }
}

#' Add a publication to the publication table
#'
#' Requires that the publication be in PubMed to auto-derive metadata such as authors, title, etc.
#' In contrast, `disease_focus` and `manifestation` need to be supplemented by the curator.
#' The `study_id` is used to get consistent `studyName` and `fundingAgency` from study table without manual input.
#'
#' @param pmid PubMed ID (*not* PMCID) of the publication to be added.
#' @param study_id Synapse id(s) of the study that are associated with the publication.
#' @param disease_focus  (Optional) The disease focus(s) that are associated with the publication.
#' @param manifestation (Optional) The manifestation(s) that are associated with the publication.
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
#'                study_table_id = "syn16787123")
#'}
#' @export
add_publication_from_pubmed <- .add_publication_from_pubmed()

#' Get publication metadata from PubMed
#'
#' @param pmid PubMed id.
#' @return If PMID found, return meta as table w/ `title` `journal` `author` `year` `pmid` `doi`.
#' @export
from_pubmed <- function(pmid) {

  res <- easyPubMed::get_pubmed_ids(pmid)
  if(res$Count == 0) {
    message(glue::glue("Nothing found for PMID:{pmid}"))
    return() # Return NULL early if no records found
  }
  p <- easyPubMed::fetch_pubmed_data(res, format = "xml", retmax = 1) %>%
    `[[`(1) %>% xml2::read_xml() %>% xml2::as_list()

  authors <- mapply(function(author) paste(author$ForeName, author$LastName),
                    p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$AuthorList)
  journal <- tools::toTitleCase(p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Journal$Title[[1]])
  title <- glue::glue_collapse(unlist(p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ArticleTitle))
  doi <- paste0("https://www.doi.org/", p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ELocationID[[1]])
  year <- p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year[[1]]
  if(is.null(year)) { # when not available resort to ArticleDate with note (often doesn't matter)
   year <- p$PubmedArticleSet$PubmedArticle$MedlineCitation$Article$ArticleDate$Year[[1]]
   message(glue::glue("Note: Using article year for PMID:{pmid} because of missing journal meta. Review and modify if needed."))
  }

  record <- data.frame(title = title, journal = journal, author = I(list(authors)),
                       year = year, pmid = glue::glue("PMID:{pmid}"), doi = doi)
  return(record)
}


#' Add a batch of publications from spreadsheet
#'
#' @inheritParams add_publication_from_pubmed
#' @param file Spreadsheet (.csv/.tsv) with pubs to add should have `pmid`, `studyId`, `diseaseFocus`, `manifestation`.
#' `pmid` is one per row and unique, rest can be `list_sep` vals.
#' @param list_sep Delimiter character used to separate list columns.
#' @import data.table
#' @export
add_publications_from_file <- function(file,
                                       publication_table_id,
                                       study_table_id,
                                       list_sep = "|", dry_run = TRUE) {

  pubs <- fread(file, colClasses = "character")
  n <- nrow(pubs)
  for(col in c("studyId", "diseaseFocus", "manifestation")) {
    pubs[[col]] <- strsplit(pubs[[col]], split = list_sep, fixed = TRUE)
  }

  add_pub <- .add_publication_from_pubmed(batch = n)
  for(i in 1:n) {
    new_pubs <- add_pub(pmid = pubs$pmid[i],
                        study_id = pubs$studyId[[i]],
                        disease_focus = pubs$diseaseFocus[[i]],
                        manifestation = pubs$manifestation[[i]],
                        publication_table_id = publication_table_id,
                        study_table_id = study_table_id,
                        dry_run = dry_run)
  }
  new_pubs
}
