# TEMPLATES --------------------------------------------------------------------#

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

#' Simple bipartite representation in mermaid charts
#'
#' @param nodeset1 Character vector of one or more node ids. If named, nodes will use names instead of ids as labels.
#' @param nodeset2 Character vector of one or more node ids. If named, nodes will use names instead of ids as labels.
#' @param nodeset1_title Title for nodeset1.
#' @param nodeset2_title Title for nodeset2.
#' @param links Optional, character vector of edges between nodes.
#' @export
bipartite_mmd_template <- function(nodeset1,
                                   nodeset2,
                                   nodeset1_title = "INPUT",
                                   nodeset2_title = "OUTPUT",
                                   links = "") {

  theme <- "%%{init: {'themeVariables': { 'primaryColor': '#125e81','edgeLabelBackground': 'white' }}}%%"
  glue::glue(
    "
  graph LR

      {theme}

      classDef Dataset fill:#625191,color:#fff,stroke-width:0px
      classDef Folder fill:#e9b4ce,color:#000,stroke-width:0px

      style SET1 fill:#f2f7f9,stroke-width:0px
      style SET2 fill:#b4cbd9,stroke-width:0px

      subgraph SET1 [{nodeset1_title}]
        {nodeset1}
      end

      subgraph SET2 [{nodeset2_title}]
        {nodeset2}
      end

      %% links
      {links}

    ")
}

#' Wrapper to create Data Sharing Plan to project dataset comparison chart
#'
#' @param dsp_datasets Named vector of datasets in data sharing plan.
#' @param project_datasets Named vector of datasets in project.
#' @export
dsp_dataset_mapping <- function(dsp_datasets, project_datasets) {

  dsp_nodes <- as_mmd_node(dsp_datasets, class = "Dataset")
  project_nodes <- as_mmd_node(project_datasets, class = "Folder")
  # This relies on matching by name for now
  pairings <- pmatch(names(dsp_datasets), names(project_datasets))
  pairings <- pairings[!is.na(pairings)]
  if(length(pairings)) {
    links <- as_mmd_link(dsp_datasets[pairings],
                         project_datasets[pairings],
                         style = "dash")
  }

  bipartite_mmd_template(nodeset1 = dsp_nodes,
                         nodeset2 = project_nodes,
                         nodeset1_title = "DSP",
                         nodeset2_title = "Project",
                         links = links)

}

# Helpers ----------------------------------------------------------------------#

#' Generate notation for mermaid.js nodes
#'
#' @param entity Character vector of one or more entity ids.
#' If named, nodes will use names instead of ids as labels. Note that entity ids
#' starting with "_" are considered blank nodes and are treated specially.
#' @param class Optional, add a class to the node.
#' @keywords internal
as_mmd_node <- function(entity,
                        class = c("Project", "Dataset", "Folder")) {

  class <- match.arg(class)
  nodes <- list()

  for(i in seq_along(entity)) {
    id <- entity[i]
    if(grepl("^_", id)) {
      name <- "??"
      nodes[[id]] <- glue::glue("{id}[{name}]:::Missing;\n")
    } else {
      name <- names(entity)[i]
      if(is.null(name)) name <- id
      nodes[[id]] <- glue::glue("{id}[{name}]:::{class};\n")
    }
  }

  nodes <- unlist(nodes, use.names = F) %>%  glue::glue_collapse("\n")
  nodes

}

#' Generate notation for mermaid.js link
#'
#' @param n1 Id for node at one end.
#' @param n2 Id for node at other end.
#' @param directional Boolean option for diretional arrow from n1 to n2.
#' @param style Option for "solid" or "dash"-style links.
#' @keywords internal
as_mmd_link <- function(n1, n2, directional = TRUE, style = "solid") {

  link <- if(style == "solid") "--" else "-.-"
  link <- if(directional) paste0(link, "> ") else paste0(link, " ")
  links <- Map(function(x1, x2) paste0(x1, link, x2, ";"), n1, n2) %>%
    unlist(use.names = F) %>%
    glue::glue_collapse("\n")
}

#' Wrapper to create data-driven flowchart with pretty processing provenance mermaid template
#'
#' This generates a mermaid file, which can be rendered live/further edited in
#'
#' @param report Which report to determine subset of data for which to generate flowchart fig.
#' @examples
#' \dontrun{
#' flowchart <- processing_flowchart(report = "2023-MY")
#' cat(flowchart, file = "flowchart.mmd")
#'}
#' @export
processing_flowchart <- function(report) {

  # Get data
  data <- .syn$tableQuery(glue::glue("SELECT projectId,inputDataset,inputDataType,workflow,publishDir,processedDatasets FROM syn34627613 WHERE report='{report}'"))
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
  projects <- unique(data$projectId)
  projects <- setNames(projects, sapply(projects, function(x) .syn$get(x)$properties$name))
  project_nodes <- as_mmd_node(projects, class = "Project")
  input_dataset_nodes <- as_mmd_node(data$inputDataset, class = "Dataset")
  folder_nodes <- as_mmd_node(data$publishDir, class = "Folder")
  output_dataset_nodes <- as_mmd_node(data$processedDatasets, class = "Dataset")

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




