#' Add a preprint to the publication table.
#' @description Add a preprint to the publication table.
#' @param publication_table_id The synapse id of the portal publication table. Must have write access.
#' @param doi The DOI of the preprint to be added.
#' @param study_name The name(s) of the study that are associated with the publication.
#' @param study_id The synapse id(s) of the study that are associated with the publication.
#' @param funding_agency The funding agency(s) that are associated with the publication.
#' @param disease_focus The disease focus(s) that are associated with the publication.
#' @param manifestation The manifestation(s) that are associated with the publication.
#' @param dry_run Default = TRUE. Skips upload to table and instead prints formatted publication metadata.
#' @return If dry_run == T, returns publication metadata to be added.
#' @examples add_preprint(publication_table_id = 'syn16857542',
#'               doi = '10.1074/jbc.RA120.014960',
#'                study_name = c(toJSON("Synodos NF2")),
#'                study_id = c(toJSON("syn2343195")),
#'                funding_agency = c(toJSON("CTF")),
#'                disease_focus = c(toJSON("Neurofibromatosis 2")),
#'                manifestation = c(toJSON("Meningioma")),
#'                dry_run = F)
#' @export
#'
add_publication <- function(publication_table_id, email_address, doi, study_name, study_id, funding_agency, disease_focus, manifestation, dry_run = T){

  #TODO: Check schema up-front and convert metadata to json in correct format

    pub_table <- syn$tableQuery(glue::glue('select * from {publication_table_id}'))$asDataFrame()

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

        if(dois_df$authors[[1]])

        author_list <- dois_df$authors[[1]] %>%
          select(given, family) %>%
          filter(!is.na(given) | !is.na(family)) %>%
          dplyr::transmute(name = glue::glue('{given} {family}'))

        if("name" %in% colnames(dois_df$authors[[1]])){
          consortium_list <- dois_df$authors[[1]] %>%
            filter(!is.na(name)) %>%
            select(name)
          author_list <- bind_rows(author_list, consortium_list)
        }
        #TODO: Fix this so that consortium shows up in correct position in list

        #wrapping json in c() is necessary to coerce to data frame near end of function

         author_list <- author_list %>%
            purrr::pluck('name') %>%
            jsonlite::toJSON() %>%
            c()
        ##extract other metadata
        journal <- dois_df$journal_name

        ##title case not typically used for scientific publications
        title <- dois_df$title

        ## default function doesn't get accurate publication date, but rather the listing date. use different function to get publication year:
        year <- dois_df$year

        #This function was written with preprints in mind, but should be able to parse publications too. this
        pmids <- easyPubMed::get_pubmed_ids(doi) ##query pubmed for pmid

        pmid <- pmids$IdList$Id[1]

        if(is.null(pmid)){
          pmid <- ""
          #fairly sure this is necessary for synapseclient to interpret as blank
        }

        #return metadata

        schema <- syn$get(entity = publication_table_id)

        new_data <- data.frame("title"=title, "journal"=journal, "author" = author_list, "year"=year, "pmid" = pmid, "doi"=doi,
                                 "studyName"= study_name, "studyId"=study_id,"fundingAgency"= funding_agency,"diseaseFocus"= disease_focus,
                                 "manifestation"=manifestation)

        schema <- syn$get(entity = publication_table_id)

        colnames <- pub_table %>% filter(is.na(doi))

        new_row <- bind_rows(colnames, new_data)

          if(dry_run == F){
            .store_publication(schema, new_row)
            glue::glue('{doi} added!')
          }else{
            print(new_row)
          }

      }
    }}
