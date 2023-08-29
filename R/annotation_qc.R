#' Generate manifest via schematic service
#' 
#' Note that this uses the access token of user that should already by logged in with `syn_login`.
#' 
#' @param data_type Data type of the manifest to generate (aka Component).
#' @param dataset_id Optional, if given this fills out manifest for existing dataset instead of generating a blank manifest.
#' @param title Optional, custom title.
#' @param schema_url Optional, defaults to main NF 'latest' data model.
#' @param asset_view Optional, defaults to main NF portal fileview.
#' @param output_format Format of 'excel', 'google_sheet', or 'dataframe'. Defaults to 'excel'.
#' @param use_annotations Use annotations if filling out manifest for existing dataset. Defaults to TRUE for NF.
#' @param service Service endpoint to use. Defaults to the schematic production endpoint.
#' @returns For excel, path to local file; for google_sheet, URL to sheet; for dataframe, JSON string of dataframe data.
manifest_generate <- function(data_type,
                              dataset_id = NULL,
                              title = data_type,
                              schema_url = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                              asset_view = "syn16858331", 
                              output_format = "excel",
                              use_annotations = TRUE,
                              service = "https://schematic.api.sagebionetworks.org/v1/manifest/generate") {
  
  # yes, param has been re-encoded like this for 'dataframe'
  output_format_param <- if (output_format == "dataframe") "dataframe (only if getting existing manifests)" else output_format
  access_token <- .syn$credentials$secret
  use_annotations <- tolower(as.character(use_annotations))
  
  req <- httr::GET(service,
                   query = list(
                     schema_url = schema_url,
                     title = title,
                     data_type = data_type,
                     use_annotations = use_annotations,
                     dataset_id = dataset_id,
                     asset_view = asset_view,
                     output_format = output_format_param,
                     access_token = access_token
                   ))
  
  status <- httr::status_code(req)
  if(status != 200L) stop("Unsuccessful request, received status code: ", status)
  
  if(output_format == "excel") {
    file <- "manifest.xlsx"
    message(glue::glue("Manifest generated and saved as {file}"))
    bin <- httr::content(req, "raw")
    writeBin(bin, file)
    return(file)
  }
  
  if(output_format == "google_sheet") {
    message("Manifest generated as Googlesheet(s)")
    url <- httr::content(req)
    return(url)
  } 
  
  if(output_format == "dataframe") {
    message("Manifest(s) generated as JSON doc")
    json_str <- jsonlite::minify(httr::content(req, "text", encoding = "UTF-8"))
    return(json_str)
  }
}

#' Validate manifest via schematic service
#' 
#' Get validation results from schematic service. Some downstream utils consume these results for custom display or summary. 
#' NOTE: This doesn't support files, though the endpoint supports uploading `.csv`. 
#'
#' @inheritParams manifest_generate
#' @param json_str JSON string representing metadata.
#' @param restrict_rules Use only basic schematic validation instead of extended validation with Great Expectations, default `FALSE`.
manifest_validate <- function(data_type,
                              json_str = NULL,
                              restrict_rules = FALSE,
                              schema_url = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                              service = "https://schematic.api.sagebionetworks.org/v1/model/validate") {
  
  restrict_rules <- tolower(as.character(restrict_rules))
  req <- httr::POST(service,
                    query = list(
                     schema_url = schema_url,
                     data_type = data_type,
                     restrict_rules = restrict_rules,
                     json_str = json_str
                   ))
  
  status <- httr::status_code(req)
  if(status != 200L) stop("Unsuccessful request, received status code: ", status)
  result <- httr::content(req)
  result
}

#' Provide a pass/fail summary result
#' 
#' @param result Result list data from schematic service. 
#' @returns Boolean for whether passed.
#' @returns List of structure `list(result = result, notes = notes)`, where `result` indicates whether the dataset passed.
#' @keywords internal
manifest_passed <- function(result) {
  
  errors <- length(result$errors)
  if(errors) {
    messages <- unique(sapply(result$errors, function(x) x[[3]]))
    notes <- paste(messages, collapse = ", ")
    return(list(result = FALSE, notes = notes))
  } else {
    return(list(result = TRUE, notes = "No errors"))
  }
}

#' Infer data type of a dataset folder
#' 
#' Infer the data type by checking the first few files. 
#' TODO: Check `dataType` instead of Component and derive Component 
#' because some older files does not have Component explicitly.
#' 
#' @inheritParams manifest_generate
#' @return List of structure `list(result = result, notes = notes)`, where `result` can be `NA`.
#' @export
infer_data_type <- function(dataset_id) {
  
  children <- .syn$getChildren(dataset_id)
  children <- reticulate::iterate(children)
  if(!length(children)) return(list(result = NA, notes = "Empty dataset folder"))
  children <- first(children, 3)
  data_type <- c()
  for (entity in children) {
    e <- .syn$getAnnotations(entity)
    data_type <- append(data_type, e$Component)
  }
  data_type <- unique(data_type)
  if(is.null(data_type)) return(list(result = NA, notes = "Metadata insufficient to infer data type."))
  if(length(data_type) > 1) return(list(result = NA, notes = "Conflicting data types observed."))
  return(list(result = data_type, notes = ""))
}


#' QC dataset metadata with pass/fail result
#' 
#' For use in R programmatic workflows, this wraps `schematic` for simple validation.
#' A dataset in this context is a folder, usually tagged with `contentType` = "dataset".
#' This is considered alpha and is likely to change based on changes in `schematic`. 
#' 
#' Note that we prefer to wrap the schematic web API over a local installation because:
#' - Will not require user to go through local schematic setup for this to be functional
#' - API more likely reflects an up-to-date version of schematic and consistent with current DCA deployment
#' 
#' When `data_type` can't be inferred based on annotations, this is treated as a fail.
#' 
#' @param dataset_id Id of folder that represents a dataset, not actual Synapse dataset entity -- see details.
#' @param data_type A specific data type to validate against, otherwise tries to infer based on annotations. See details.
#' @param asset_view A reference view, defaults to the main NF portal fileview.
#' @param schema_url Schema URL, points by default to 'latest' main NF schema, can change to use a specific released version.
#' @returns List of structure `list(result = result, notes = notes)`, where `result` indicates whether the dataset passed.
#' @export
meta_qc_dataset <- function(dataset_id, 
                            data_type = NULL,
                            asset_view = "syn16787123",
                            schema_url = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld") {
  
  if(is.null(data_type)) {
    data_type <- infer_data_type(dataset_id)$result
    if(is.na(data_type)) return(list(result = FALSE, notes = "Metadata quality insufficient to even infer data type."))
  }
  
  # Reconstitute metadata manifest as JSON doc
  json_str <- manifest_generate(data_type, dataset_id, output_format = "dataframe")
  
  # Validate
  results <- manifest_validate(data_type = data_type, json_str = json_str)
  manifest_passed(results) 
  
}


#' QC metadata at the project level with pass/fail result 
#' 
#' For projects with a relatively standard structure that also corresponds to what the DCA expects, 
#' this is an adequate wrapper to go through the datasets and do basic QC in a one-stop-shop manner.
#' For selective validation or other (e.g. milestone-based or ad hoc) structures, look at `meta_qc_dataset`.
#' 
#' @param project_id Synapse project id.
#' @return A table of with rows for the datasets QC'd, with dataset id, name, TRUE/FALSE pass result, and summary from validation result.
meta_qc_project <- function(project_id) {
  
  data_root <- find_data_root(project_id)
  if(is.null(data_root)) {
    messsage("Data root could not be located. Project structure may require custom assessment and dropping down to `meta_qc_dataset`.")
    return(NA)
  }
  in_data <- .syn$getChildren(data_root)
  in_data <- reticulate::iterate(in_data)
  # Select only folders in data and ignore files at this level
  datasets <- Filter(function(x) x$type == "org.sagebionetworks.repo.model.Folder", in_data)
  if(length(datasets)) {
    dataset_names <- sapply(datasets, `[[`, "name")
    dataset_ids <- sapply(datasets, `[[`, "id")
    message("Datasets for QC:\n", glue::glue_collapse(dataset_names, sep = "\n"))
    results <- lapply(dataset_ids, meta_qc_dataset)
    report <- rbindlist(results)
    report$dataset_name <- dataset_names
    report$dataset_id <- dataset_ids
  
  } else {
    messsage("No datasets found under data root. They may be somewhere else?")
    return(NA)
  }
}

