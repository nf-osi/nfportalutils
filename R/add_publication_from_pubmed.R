#' Add a publication to the publication table.
#' @description Add a publication to the publication table. Requires that the publication be listed in PubMed. For parameter-provided metadata (e.g. "studyName"), function must a JSON-formatted character vector if the destination Synapse column is of "STRING_LIST" format. Currently, this function does not evaluate the schema, so this must be checked manually.
#' @param publication_table_id The synapse id of the portal publication table. Must have write access.
#' @param pmid The PubMed ID (*not* PMCID) of the publication to be added.
#' @param study_name The name(s) of the study that are associated with the publication.
#' @param study_id The synapse id(s) of the study that are associated with the publication.
#' @param funding_agency The funding agency(s) that are associated with the publication.
#' @param disease_focus The disease focus(s) that are associated with the publication.
#' @param manifestation The manifestation(s) that are associated with the publication.
#' @param dry_run Default = TRUE. Skips upload to table and instead prints formatted publication metadata.
#' @return If dry_run == T, returns publication metadata to be added.
#' @examples add_publication_from_pubmed(publication_table_id = 'syn16857542',
#'                pmid = '33574490',
#'                study_name = c(toJSON("Synodos NF2")),
#'                study_id = c(toJSON("syn2343195")),
#'                funding_agency = c(toJSON("CTF")),
#'                disease_focus = "Neurofibromatosis 2",
#'                manifestation = c(toJSON("Meningioma")),
#'                dry_run = T)
#' @export
#'
add_publication_from_pubmed <- function(publication_table_id, pmid, study_name, study_id, funding_agency, disease_focus, manifestation, dry_run = T){
   .check_login()
  #TODO: Check schema up-front and convert metadata to json in correct format

  pub_table <- .syn$tableQuery(glue::glue('select * from {publication_table_id}'))$filepath %>%
    readr::read_csv(na=character()) ##asDataFrame() & reticulate return rowIdAndRowVersion as concatenated rownames, read_csv reads them in as columns

    if(pmid %in% pub_table$pmid){
      print("publication already exists in destination table!")
    }else{

      pmids <- easyPubMed::get_pubmed_ids(pmid) ##query pubmid for pmid
      if(pmids$Count == 0){ ##if no records found, return vector of NAs
        print('nothing found for doi')
      }else{ ##otherwise look for all data
        pmids <- easyPubMed::fetch_pubmed_data(pmids, format = "xml", retmax = 1)

        pmids_df <- pmids %>%
          easyPubMed::article_to_df()

        author_list <- pmids_df %>% tidyr::unite(name, firstname, lastname, sep = " ") %>%
          purrr::pluck('name') %>% jsonlite::toJSON() %>% c()
        #wrapping json in c() is necessary to coerce to data frame near end of function

        ##extract other metadata
        ##journal names are not stored on pubmed in title case, so let's do that
        journal <- pmids_df$journal %>% unique %>% tools::toTitleCase(.)

        ##title case not typically used for scientific publications
        title <- pmids_df$title %>% unique

        ##doi
        doi <- pmids_df$doi %>% unique

        if(doi %in% pub_table$doi){
          print("publication already exists in destination table!")
        }else{

        ## default function doesn't get accurate publication date, but rather the listing date. use different function to get publication year:
        year <- easyPubMed::custom_grep(pmids, tag = "PubDate")[1] %>%
          stringr::str_extract(., "<Year>\\d+") %>%
          stringr::str_extract('\\d+') %>%
          as.double()

        pmid <- pmids_df$pmid %>% unique %>% as.double()

        if(length(journal)>1 | length(title)>1 | length(year)>1 | length(author_list)>1 | length(pmid)>1){
          print("one to many mappings detected - manually curate")
        }else{
          #return metadata

          schema <- .syn$get(entity = publication_table_id)

          new_data <- tibble::tibble("title"=title, "journal"=journal, "author" = author_list, "year"=year, "pmid" = pmid, "doi"=doi,
                                 "studyName"= study_name, "studyId"=study_id,"fundingAgency"= funding_agency,"diseaseFocus"= disease_focus,
                                 "manifestation"=manifestation)

          schema <- .syn$get(entity = publication_table_id)

          colnames <- pub_table[0,]

          new_row <- dplyr::bind_rows(colnames, new_data)

          if(dry_run == F){
            .store_publication(schema, new_row)
            glue::glue('{pmid} added!')
          }else{
            print(new_row)
          }

        }
        }
      }
  }}
