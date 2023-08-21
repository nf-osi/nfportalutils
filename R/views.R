#' Create a view
#'
#' This creates a generic view, including by default just file entities and the default columns
#' (i.e. defaults to a generic fileview).
#' This is often useful to get ids of files for a large number of nested files by creating a temp fileview
#' (the alternative is to use `walk`, but if the tree structure is not regular it can be messy to parse the output).
#' 
#' @param scope Character id(s) of project or folder container(s) in scope.
#' @param project Parent project id to create the view in.
#' @param name Name of view.
#' @param include Which entity type(s) to include in scope. Defaults to files. 
#' 
new_view <- function(scope, 
                     project, 
                     name = "New View", 
                     include = "FILE") {
  
  included <- match.arg(include, several.ok = TRUE)
  view <- synapseclient$EntityViewSchema(
    name = name,
    columns = list(),
    parent = project,
    scopes = scope,
    includeEntityTypes = list(include),
    add_default_columns = TRUE)
  view <- .syn$store(view)
  invisible(view)
}
