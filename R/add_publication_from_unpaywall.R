#' Add a publication or preprint to the publication table via the Unpaywall API.
#' @description Add a publication to the publication table. Publication must be in unpaywall database to retrieve info. For parameter-provided metadata (e.g. "studyName"), function must a JSON-formatted character vector if the destination Synapse column is of "STRING_LIST" format. Currently, this function does not evaluate the schema, so this must be checked manually.
#' @param publication_table_id The synapse id of the portal publication table. Must have write access.
#' @param email_address A valid email address. Is used to request metadata from the Unpaywall API. Please change the example to a real email address to help Unpaywall accurately track usage.
#' @param doi The DOI of the preprint to be added.
#' @param is_preprint Default = FALSE. Set to TRUE if DOI is from a preprint.
#' @param preprint_server Provide preprint server name. Must be one of 'bioRxiv', 'medRxiv', 'chemRxiv', 'arXiv'
#' @param study_name The name(s) of the study that are associated with the publication.
#' @param study_id The synapse id(s) of the study that are associated with the publication.
#' @param funding_agency The funding agency(s) that are associated with the publication.
#' @param disease_focus The disease focus(s) that are associated with the publication.
#' @param manifestation The manifestation(s) that are associated with the publication.
#' @param dry_run Default = TRUE. Skips upload to table and instead prints formatted publication metadata.
#' @note
#' @return If dry_run == T, returns publication metadata to be added.
#' @examples
#' \dontrun{  
#' add_publication_from_unpaywall(publication_table_id = 'syn16857542',
#'                email_address = 'foo@bar.com',
#'                doi = '10.1074/jbc.RA120.014960',
#'                study_name = c(toJSON("Synodos NF2")),
#'                study_id = c(toJSON("syn2343195")),
#'                funding_agency = c(toJSON("CTF")),
#'                disease_focus = "Neurofibromatosis 2",
#'                manifestation = c(toJSON("Meningioma")),
#'                dry_run = T)
#'}                
#' @export
#'
add_publication_from_unpaywall <- function(publication_table_id,
                            email_address,
                            doi,
                            is_preprint = F,
                            preprint_server = NULL,
                            study_name,
                            study_id,
                            funding_agency,
                            disease_focus,
                            manifestation,
                            dry_run = T){

  #TODO: Check schema up-front and convert metadata to json in correct format

  .check_login()

    schema <- .syn$get(entity = publication_table_id)

    pub_table <- .syn$tableQuery(glue::glue('select * from {publication_table_id}'))$filepath %>%
      readr::read_csv(na=character()) ##asDataFrame() & reticulate return rowIdAndRowVersion as concatenated rownames, read_csv reads them in as columns

    if(doi %in% pub_table$doi){
      print("doi already exists in destination table!")
    }else{

      dois_df <- roadoi::oadoi_fetch(dois = doi,
                                     email = email_address) #query unpaywall for doi

      if(nrow(dois_df)==0){ ##if no records found, exit
        print('nothing found for doi')
      }else if(nrow(dois_df)>1){
          print('multiple matches found for doi, aborting')
      }else{
        ##otherwise look for all data

        author_data <- dois_df$authors[[1]]

        ##column "name" is returned by roadoi with consortium authors
        if(all(c("given", "family", "name") %in% colnames(author_data))){
          author_list <- tidyr::unite(author_data, "names", "given", "family", "name",
                                      na.rm = T, sep = " ")
        }

        ##when no consortium author present, just use given/family names
        if(all(c("given", "family") %in% colnames(author_data))){
          author_list <- tidyr::unite(author_data, "names", "given", "family",
                                                     na.rm = T, sep = " ")
        }

        #wrapping json in c() is necessary to coerce to data frame near end of function
         author_list <- author_list %>%
            purrr::pluck('names') %>%
            jsonlite::toJSON() %>%
            c()

        ##extract other metadata
         if(is_preprint == T){
           valid_preprint_servers <- c('bioRxiv', 'medRxiv', 'chemRxiv', 'arXiv')
           if(preprint_server %in% valid_preprint_servers){
           journal <- preprint_server
           }else{
             glue::glue('preprint_server must be one of {glue::glue_collapse(valid_preprint_servers, sep = ", ")}')
           }
         }else{
           journal <- dois_df$journal_name
         }

        ##title case not typically used for scientific publications
        title <- dois_df$title

        ## default function doesn't get accurate publication date, but rather the listing date. use different function to get publication year:
        year <- dois_df$year %>% as.double()

        pmids <- easyPubMed::get_pubmed_ids(doi) ##query pubmed for pmid

        pmid <- pmids$IdList$Id[1] %>% as.double()

        if(is.null(pmid) | length(pmid)==0){
          pmid <- NA
        }
        
        doi_url <- glue::glue("www.doi.org/{doi}")

        #return metadata
        new_data <- tibble::tibble("title"=title, "journal"=journal, "author" = author_list, "year"=year, "pmid" = pmid, "doi"=doi_url,
                                 "studyName"= study_name, "studyId"=study_id,"fundingAgency"= funding_agency,"diseaseFocus"= disease_focus,
                                 "manifestation"=manifestation)

        colnames <- pub_table %>% dplyr::filter(is.na(doi))

        new_row <- dplyr::bind_rows(colnames, new_data)

          if(dry_run == F & nrow(new_row) > 0){
            .store_rows(schema, new_row)
            glue::glue('{doi} added!')
          }else if(nrow(new_row) == 0){
            'error in generating new row, aborting'
          }else{
            new_row
          }

      }
    }}

#' Adds a row to a table.
#' @param schema A synapse table Schema object.
#' @param new_row A data frame of one or more rows that match the provided schema.
#' @export
.store_rows <- function(schema, new_row){

  table <- .syn$store(synapseclient$Table(schema, new_row))

}

# .pluck_column_type_and_name <- function(column){
#   coltype <- purrr::pluck(column, "columnType")
#   name <- purrr::pluck(column, "name")
#
#   c(coltype, name)
# }

