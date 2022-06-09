#' Look up connected nodes by specified property in JSON-LD schema
#'
#' Given `@id`, looks up connected nodes by specified property (e.g. 'sms:something').
#' Intended to be a generic used to define more specific lookup conveniences.
#' Can do recursive lookup (which makes sense for certain properties such as "dependsOn"
#' but doesn't make sense for properties such as "rdfs:label");
#' this assumes that the graph is actually a tree (acyclic).
#' Also the JSON-LD schema is assumed to be schematic-generated, 
#' and this might not work with JSON-LD generated from other tools such as LinkML
#' due to small differences and hard-coded stuff.
#' @param id Id (`@id`) for which to get range values; include prefix if needed. 
#' @param prop Property; include prefix if needed.
#' @param schema Path (URL or local) to file from which schema will be read, or schema as list object.
#' @param return_labels Return labels (default), otherwise ids of connected nodes. 
#' @param recursive Recursive lookup?
#' @param result Vector of accumulated results; used for recursive lookup.
#' @param rest Vector of remaining ids; used for recursive lookup.
#' @export
get_by_prop_from_json_schema <- function(id, 
                                         prop,
                                         schema = 'https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld',
                                         return_labels = TRUE,
                                         recursive = FALSE,
                                         result = NULL,
                                         rest = NULL) {
  if(!is.list(schema)) {
    schema <- jsonlite::read_json(schema)
    schema <- schema$`@graph`
  }

  matches <- Filter(function(x) x$`@id` == id, schema)
  if(!length(matches)) stop("Id not found in schema!")
  ids <- unlist(lapply(matches[[1]][[prop]], function(x) x$`@id`))
  
  if(return_labels) {
    labels <- Filter(function(x) x$`@id` %in% ids, schema) %>%
      sapply(function(x) x$`sms:displayName`) %>% unlist()
    result <- c(result, labels) 
  } else {
    result <- c(result, ids)
  }
  
  rest <- c(rest, ids)
  if(recursive && length(rest)) {
    id <- rest[1]
    rest <- rest[-1]
    get_by_prop_from_json_schema(id, 
                                 prop,
                                 schema, 
                                 return_labels, 
                                 recursive,
                                 result, 
                                 rest)
  } else {
    # result
    unique(result) # dup ids exist because of https://github.com/Sage-Bionetworks/schematic/issues/708
  }
}

#' Get dependencies for node in JSON-LD schema
#' 
#' Shorthand that uses `get_by_prop_from_json_schema` under the hood;
#' intended usage includes checking properties defined by an annotation template.
#' @inheritParams get_by_prop_from_json_schema
#' @export
get_dependency_from_json_schema <- function(id,
                                            prop = "sms:requiresDependency",
                                            schema = 'https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld',
                                            return_labels = TRUE,
                                            recursive = TRUE,
                                            result = NULL,
                                            rest = NULL) {
  
  get_by_prop_from_json_schema(id, prop, schema, return_labels, recursive, result, rest)
  
}
