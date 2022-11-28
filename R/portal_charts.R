#' Pretty processing provenance mermaid template for the portal
#' 
#' Mermaid flowchart helps navigate processed data with interactive links by default.
#' See also https://github.com/mermaid-js/mermaid.
#' 
#' @keywords internal
ppp_mmd_template <- function(project_nodes,
                             input_dataset_nodes,
                             folder_nodes,
                             output_dataset_nodes,
                             workflow_links,
                             dataset_links,
                             clicks
                        ) {

  theme <- "%%{init: {'themeVariables': { 'primaryColor': '#125e81','edgeLabelBackground': 'white' }}}%%"
  glue::glue(
  "
  graph LR
  
      {theme}
      
      classDef Project fill:#125e81,color:#fff,stroke-width:0px
      classDef Dataset fill:#625191,color:#fff,stroke-width:0px
      classDef Folder fill:#e9b4ce,color:#000,stroke-width:0px
      classDef Missing fill:red,color:#fff,stroke-width:0px
      
      style IN fill:#f2f7f9,stroke-width:0px
      style OUT fill:#b4cbd9,stroke-width:0px
      style PD fill:#bddfe0,stroke-width:0px
      
    %% project nodes
      {project_nodes}
    
    %% input dataset nodes
    subgraph IN [input raw dataset]
      {input_dataset_nodes}
    end
    
    %% publish dirs
    subgraph OUT [fa:fa-folder result directory]
      {folder_nodes}
    end
    
    %% processed dataset nodes
    subgraph PD [processed datasets]
      {output_dataset_nodes}
    end
    
    %% links
    {workflow_links}
    
    %% dataset links
    {dataset_links}
    
  linkStyle default color:#27ae60
  {clicks}
  ")
}

#' Helper function for rendering nodes
#' 
#' @keywords internal
as_mmd_node <- function(entity,
                    named = FALSE,
                    class = c("Project", "Dataset", "Folder")) {
  
  class <- match.arg(class)
  # Recognize NaN as class=Missing
  if(grepl("^_", entity)) {
    name <- "??"
    id <- entity
    class <- "Missing"
  } else if(named) {
    e <- .syn$get(entity, downloadFile = F)
    name <- e$properties$name
    id <- entity
  } else {
    id <- name <- entity
  }
  
  glue::glue("{id}[{name}]:::{class};\n")
}


#' Wrapper to create data-driven flowchart with pretty processing provenance mermaid template
#' 
#' @param year Year to determine subset of data for which to generate flowchart. 
#' @examples
#' \dontrun{
#' flowchart <- processing_flowchart(year = 1)
#' cat(flowchart, file = "flowchart.mmd")
#'}
processing_flowchart <- function(year = 1) {
  
  # Get data
  data <- .syn$tableQuery(glue::glue('SELECT projectId,inputDataset,inputDataType,workflow,publishDir,processedDatasets FROM syn34627613 WHERE "year"={year}'))
  data <- data$asDataFrame()
  
  # Replace NaN with blank nodes
  options(digits.secs = 6)
  for(col in names(data)) {
    for(j in seq_along(data[[col]])) { # don't vectorize bc we're using Sys.time to generate ids
      if(is.na(data[[col]][j][1])) data[[col]][j] <- paste0("_", as.double(Sys.time()))
    }
  }
  
  collapse <- function(x) unlist(x, use.names = F) %>%  glue::glue_collapse("\n")
  
  # Only project nodes use names of the project as labels, while others do not to keep chart relatively clean
  project_nodes <- sapply(unique(data$projectId), as_mmd_node, named = TRUE, class = "Project") %>% collapse()
  input_dataset_nodes <- sapply(data$inputDataset, as_mmd_node, class = "Dataset") %>% collapse()
  folder_nodes <- sapply(data$publishDir, as_mmd_node, class = "Folder") %>% collapse()
  output_dataset_nodes <- sapply(unlist(data$processedDatasets), as_mmd_node, class = "Dataset") %>% collapse()
  
  workflow_links <- paste0(data$projectId, " --> ", data$inputDataset, " -->|", data$workflow, "| ", data$publishDir, ";") %>% collapse()
  dataset_links <- Map(function(p, d) paste0(p, " --> ", d, ";"), data$publishDir, data$processedDatasets) %>% collapse()
  
  # Make all valid syn accessions clickable
  syn_links <- grep("^syn", unlist(data, use.names = F), value = T)
  clicks <- sapply(syn_links, function(x) glue::glue('click {x} "https://www.synapse.org/#!Synapse:{x}" "Go to accession" _blank')) %>% collapse()
  
  ppp_mmd_template(project_nodes,
                   input_dataset_nodes, 
                   folder_nodes,
                   output_dataset_nodes,
                   workflow_links,
                   dataset_links,
                   clicks)
}




