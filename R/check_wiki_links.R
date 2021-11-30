#' Check wiki links
#' 
#' @description This primarily supports wiki quality control. 
#' The method wraps some helpers to retrieve the wiki content for the given project(s), extract URL(s) from the content, and return a list of link check results per project wiki. 
#' Note that only the main wiki page is checked. As well, this does not remove/replace the problematic link(s), and there still may be false positive/negatives that may need to be reviewed manually.
#' @inheritParams get_project_wiki
#' @param to_table `TRUE` to return results as table or else keep as a list. Additional downstream operations may prefer one or the other.
#' @return Depending on `to_table`, a list or tibble of projects with links and check results for links. The list will include projects without links (as an empty list), while the table will omit projects without links.
#' @examples 
#' \dontrun{
#' check_wiki_links(project_id = c("syn11374354","syn2343195"))
#'}
#' @export
check_wiki_links <- function(project_id, to_table = TRUE) {
  .check_login()
  
  texts <- lapply(project_id, function(x) try(get_project_wiki(x)))
  names(texts) <- project_id
  urls <- lapply(texts, function(txt) regmatches(txt, gregexpr("https?://[^ )}\n]+|url=\\K[^ )}\n]+", txt, perl = T))) %>% 
    unlist(recursive = F)
  # need to decode or RCurl will complain on some that are valid 
  urls_decoded <- lapply(urls, function(x) sapply(x, function(u) if(length(u)) utils::URLdecode(u) else NULL, USE.NAMES = F))
  urls_result <- lapply(urls_decoded, bad_url)
  if(to_table) {
    urls_result <- urls_result[lengths(urls_result) != 0] 
    urls_result <- tibble(project = names(urls_result), result = urls_result) %>%
      tidyr::unnest_longer(result, indices_to = "url", indices_include = TRUE)
  }
  urls_result
}

#' Get wiki content of synapse project(s)
#'
#' @description Get the wiki object or text content (main page only). This is primarily a helper function used in QC but may be useful for other wiki analysis.
#' @param project_id Character vector of synapse project id(s) for which to get wiki.
#' @param markdown If `TRUE` (default) return only the markdown text, else return full wiki object.
#' @return A list storing the wiki object or markdown-formatted text.
#' @examples 
#' \dontrun{ 
#' txt <- get_project_wiki(c("syn11374354","syn2343195"))
#'}
#' @export
get_project_wiki <- function(project_id, markdown = TRUE) {
  .check_login()
  
  wiki <- .syn$getWiki(project_id)
  if(markdown) wiki$markdown else wiki
}

#' Helper function to check urls
#' 
#' @description Check whether URL(s) return a HTTP error, indicating a broken link. 
#' Note that this uses `curl` under the hood, which may give timeout errors and therefore false positives for some links that are valid but take too long to resolve.
#' @param url A character vector of one or more URLs.
#' @return Result vector of same size with values "bad" or "ok". 
#' @export
bad_url <- function(url) {
  result <- sapply(url, function(x) tryCatch(httr::http_error(x, httr::config(followlocation = FALSE)), error = function(e) return("bad")))
  # can get RCurl errors which doesn't mean url is bad, e.g. timeout errors
  result <- sapply(result, function(x) switch(as.character(x), "TRUE" = "bad", "FALSE" = "ok", as.character(x)))
  result
}
