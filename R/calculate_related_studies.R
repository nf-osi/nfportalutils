#' Calculate and add related studies to study table
#' 
#' Processes study summary text to identify clusters of related studies. 
#' Calculates tf-idf values for 1 and 2 length ngrams, and clusters studies using the ward.D clustering method. 
#' Updates data in a STRINGLIST column "relatedStudies."
#' 
#' @param study_table_id The synapse id of the portal study table. Must have write access.
#' @param n_clust Target number of clusters to generate using hierarchical clustering. 
#' In practice, the number of total summaries divided by 3 is a good starting point (100 studies = 33 clusters).
#' If given `n_k` is ignored.
#' @param n_k Generate target number of most closely related studies using k-nearest-neighbors instead;
#' since the number of desired related studies is specified, this may be preferable over using `n_clust`. 
#' Ignored if `n_clust` is already given.
#' @param dry_run Default = TRUE. Skips upload to table and instead prints study tibble.
#' @return If dry_run == T, returns study tibble and skips upload.
#' @examples
#' \dontrun{ 
#'  calculate_related_studies(study_table_id = "syn16787123",
#'                            n_clust = 36,
#'                            dry_run = T)
#'}
#' @export

calculate_related_studies <- function(study_table_id, 
                                      n_clust = NULL, 
                                      n_k = NULL, 
                                      dry_run = TRUE){

  .check_login()

  ##query the study table
  query <- .syn$tableQuery(glue::glue("select studyId,summary from {study_table_id}", includeRowIdAndRowVersion=T))

  studies <- query$filepath %>%
    readr::read_csv(na=character())
  
  cdist <- calc_study_dist_dtm(studies)
  
  # Distances can be input to either hclust or knn
  if(!is.null(n_clust) && is.numeric(n_clust)) {
    
    hc <- hclust(cdist, "ward.D")
    ## then plot the cluster borders to define them
    ## then change the number of clusters if the clusters are too large
    ## probably 2-3 studies per cluster is ideal
    #break into 20 clusters - this probably should be changed for smaller or larger numbers of studies
    #I was targeting ~3-4 related studies per cluster, this seemed to work fairly well
    clustering <- cutree(hc, n_clust)
    
    if(dry_run == TRUE) {
      plot(hc, main = "Hierarchical clustering of NF Study Summaries",
           ylab = "", xlab = "", yaxt = "n")
      
      rect.hclust(hc, n_clust, border = "red")
    }
    
    # p_words <- colSums(dtm) / sum(dtm)
    #
    # cluster_words <- lapply(unique(clustering), function(x){
    #   rows <- dtm[ clustering == x , ]
    #
    #   # for memory's sake, drop all words that don't appear in the cluster
    #   rows <- rows[ , colSums(rows) > 0 ]
    #
    #   colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
    #   })
    #
    # cluster_summary <- data.frame(cluster = unique(clustering),
    #                             size = as.numeric(table(clustering)),
    #                             top_words = sapply(cluster_words, function(d){
    #                               paste(
    #                                 names(d)[ order(d, decreasing = TRUE) ][ 1:5 ],
    #                                 collapse = ", ")
    #                             }),
    #                             stringsAsFactors = FALSE)
    
    similar_studies <- clustering %>%
      tibble::as_tibble(rownames = "relatedStudies")
    
    source_studies <-  clustering %>%
      tibble::as_tibble(rownames = "studyId")
    
    ids <- dplyr::full_join(similar_studies,source_studies) %>%
      dplyr::filter(relatedStudies != studyId) %>% #remove self-association
      dplyr::group_by(studyId) %>%
      dplyr::summarise(relatedStudies = jsonlite::toJSON(relatedStudies)) %>%
      dplyr::ungroup()
    
    studies_updated <- dplyr::left_join(studies, ids)
    
  } else if(!is.null(n_k) && is.numeric(n_k)) {
    
    cdist <- as.matrix(cdist)
    nearest <- lapply(1:nrow(cdist), function(x) row.names(cdist)[FastKNN::k.nearest.neighbors(x, cdist, k = n_k)])
    studies_updated <- studies 
    studies_updated$relatedStudies <- sapply(nearest, jsonlite::toJSON)
    
  } else {
    stop("You must specify a valid value for either n_clust or n_k!")
  }
  

  if(dry_run == FALSE){
    .update_table_data(table_id = study_table_id,
                       new_data = studies_updated,
                       etag = query$etag)
  } else {
    studies_updated
  }
}

#' Calculate study distance based on summary text
#' 
#' There are different measures of similarity; 
#' this gives cosine similarity based on summary text, which is then converted to a distance matrix.
#' In the future, other methods may be used for comparison or in ensemble.
#' @param studies A `data.frame` where each row is a "document"; should have `summary` and `studyId`. 
#' @keywords internal
calc_study_dist_dtm <- function(studies) {
  ##create a document object using the study summaries
  dtm <- textmineR::CreateDtm(doc_vec = studies$summary,
                              doc_names = studies$studyId,
                              ngram_window = c(1,2),
                              stopword_vec = c(stopwords::stopwords("en"),
                                               stopwords::stopwords(source = "smart")),
                              lower = T,
                              remove_punctuation = T,
                              remove_numbers = F,
                              verbose = F)
  
  tf_mat <- textmineR::TermDocFreq(dtm)
  tfidf <- t(dtm[ , tf_mat$term ] %>% as.matrix()) * tf_mat$idf
  tfidf <- t(tfidf)
  csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
  csim <- csim %*% t(csim)
  cdist <- as.dist(1 - csim)
  return(cdist)
}

#' Replace/update table contents = input data must have ROW_ID and ROW_VERSION columns to update, otherwise will append data.
#' @param table_id The synapse id of the table to update.
#' @param new_data The updated table.
#' @param etag An etag of the latest version of the table. If not provided, will query table_id to retrieve latest etag.
#' @export

.update_table_data <- function(table_id, new_data, etag = NULL){
  schema <- .syn$get(table_id)
  if(is.null(etag)){
    etag <- .syn$tableQuery(glue::glue("select * from {table_id}"))$etag
  }
  table <- .syn$store(synapseclient$Table(schema, new_data, etag=etag))
}
