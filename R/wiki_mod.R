#' Generate button widget for a Synapse wiki
#'
#' @description Generate markup for a button widget for a Synapse project wiki.
#' Refer to widget docs at <https://help.synapse.org/docs/Wikis.1975746682.html#Wikis-WikiWidgets>.
#' Buttons should be created sparingly and strategically.
#' See \code{\link{remove_button}} in case of future regret.
#' @param label Button label text.
#' @param url URL that the button will link to.
#' @param align Button alignment, can be one of "None", "Left", "Right", or "Center" (defaults to "None").
#' @export
button_widget <- function(label,
                          url,
                          align = c("None", "Left", "Right", "Center")) {

  align <- match.arg(align)
  url <- utils::URLencode(url, reserved = T)
  button <- glue::glue("${{buttonlink?text={label}&url={url}&align={align}}}")
  button

}

#' Add markup to a project wiki
#'
#' @description Add markup to an **existing** project wiki, e.g. regular markdown, a widget, or other Synapse wiki compatible content.
#' Errors will be encountered if one tries to modify a project wiki that does not exist.
#' @param content Markdown or other markup compatible with Synapse wikis.
#' @param project_id ID of the owner Synapse project.
#' @param subpage If given a character name, will add to a new subpage of that name.
#' If `NULL`, contents will be added to the main page.
#' @param where Where to add markup on page, "top" or "bottom" (defaults to "top").
#' Only used if adding to main page, which may already have content.
#' @param dry_run Whether to return a wiki object only without actually performing update.
#' @inheritParams get_project_wiki
#' @export
wiki_mod <- function(content, project_id, subpage = NULL,
                     where = c("top", "bottom"), dry_run = TRUE) {

  .check_login()

  wiki <- get_project_wiki(project_id = project_id, markdown = FALSE)
  if(!is.null(subpage)) {
    wiki <- synapseclient$Wiki(owner = project_id,
                               markdown = content,
                               title = subpage,
                               parentWikiId = wiki$id)
  } else {
    markdown <- wiki$markdown
    where <- match.arg(where)
    if(where == "top") {
      markdown <- paste(content, markdown, sep = "\n")
    } else {
      markdown <- paste(markdown, content, sep = "\n")
    }
    wiki$markdown <- markdown
  }

  if(!dry_run) wiki <- .syn$store(wiki)
  wiki
}

#' Remove a subpage from a project wiki
#'
#' @description Removes a wiki subpage by name (header).
#' Currently, this will decline to make any mods if there is not exactly one match for the subpage.
#' If there are multiple subpages of same name, it's not clear which is the right one to remove.
#' @param subpage Name of the subpage
#' @inheritParams wiki_mod
#' @export
remove_wiki_subpage <- function(project_id, subpage) {
  wiki_meta <- .syn$getWikiHeaders(project_id) %>% unlist()
  wiki_meta <- lapply(wiki_meta, function(x) as.character(x) %>% jsonlite::fromJSON())
  subpage_id <- lapply(wiki_meta, function(x) if(!is.null(x$title) && x$title == subpage) return(x$id)) %>% unlist()
  if(length(subpage_id) == 1) {
    wiki_subpage <- .syn$getWiki(owner = project_id, subpageId = subpage_id)
    .syn$delete(wiki_subpage)
    print(glue::glue("Removed '{subpage}'"))
  } else if(length(subpage_id) > 1) {
    print("Multiple subpages match heading specified. Nothing done.")
    # Other handling in future?
  } else {
    print("No subpage match. Nothing done.")
  }
}

#' Remove button from a project wiki
#'
#' @description This provides a way to remove buttons that should no longer be present, possibly as decided by newer and wiser design decisions.
#' See also \code{\link{button_widget}}.
#' The target button is selected based on the specified text label.
#' If for some reason there are multiple buttons with the same label, all of them will be removed.
#' @param wiki The wiki object to operate on.
#' @inheritParams button_widget
#' @inheritParams wiki_mod
#' @export
remove_button <- function(wiki, label, dry_run = TRUE) {

  .check_login()

  pattern <- paste0("\\$\\{buttonlink\\?text=", label, ".*?\\}")
  wiki$markdown <- gsub(pattern, "", wiki$markdown)
  if(!dry_run) wiki <- .syn$store(wiki)
  wiki
}

#' Create NF Data Curator App subpage
#'
#' @description Convenience method to create a subpage with the default buttons for the annotation app and docs.
#' This is a highly specific method and expected to have a limited lifespan.
#' @inheritParams wiki_mod
#' @export
data_curator_app_subpage <- function(project_id, dry_run = TRUE) {

  .check_login()
  # "Canonical" name of this page
  subpage <- "NF Data Curator App"
  # Default buttons and links
  app_label <- "App"
  app_url <- "https://shiny.synapse.org/users/rallaway/NF_data_curator"
  app_btn <- button_widget(label = app_label, url = app_url)

  doc_label <- "Docs"
  doc_url <- "https://help.nf.synapse.org/NFdocs/How-to-Annotate-Data.2110947593.html"
  doc_btn <- button_widget(label = doc_label, url = doc_url)

  buttons <- paste0(app_btn, "\n", doc_btn)

  # Add buttons
  wiki <- wiki_mod(buttons, project_id, subpage = subpage, dry_run = dry_run)
}


