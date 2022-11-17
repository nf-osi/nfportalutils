#' Map out Sarek _report_ files
#' 
#' This is in the family of helper funs to annotate secondary report files from certain nextflow workflows. 
#' For Sarek, many report files are conveniently outputted in a top-level "Reports" folder, 
#' which organize reports by sample and then tool (BFCTools, FastQC, etc.). 
#' An example reference starting at the sample-level: https://www.synapse.org/#!Synapse:syn31665258
#' While most things in "Reports" can indeed generally be called a "workflow report" (a subclass of the "report" resource),
#' some files in the bamQC reports directory are misc web assets (.css, .js, .gif, etc.) used for the HTML report. 
#' HTML reports *should* have these asset files are directly embedded/bundled in the .html file, 
#' but when they don't these misc files become an extra annotation burden. 
#' Since it's debatable to call something like a .css file a report, these files are classified instead as "report asset". 
#' 
#' Unlike the other `map_*` functions, this requires a fileview instead of using `walk` and will create one.
#' 
#' @param syn_out The `Reports` output folder which is set as scope for fileview. 
#' @param project Project in which to put fileview.
#' 
map_reports_sarek <- function(syn_out, project) {
  
  view <- synapseclient$EntityViewSchema(
    name = paste("Sarek Reports", syn_out),
    columns=list(
      synapseclient$Column(name="resourceType", columnType="STRING"),
      synapseclient$Column(name="assay", columnType="STRING"),
      synapseclient$Column(name="fileFormat", columnType="STRING"),
      synapseclient$Column(name="workflow", columnType="STRING"),
      synapseclient$Column(name="workflowLink", columnType="STRING")
    ),
    parent = project,
    scopes = syn_out,
    includeEntityTypes = list(synapseclient$EntityViewType$FILE),
    addDefaultViewColumns = TRUE,
    addAnnotationColumns = FALSE)
  
  view <- .syn$store(view)
  invisible(view)
}


#' Annotate Sarek reports
#' 
#' First runs `map_reports_sarek` under the hood.
#' 
#' @inheritParams map_reports_sarek
#' @param dry_run Whether to submit annotations or just return the manifest.
#' @import data.table
annotate_reports_sarek <- function(syn_out, project, dry_run) {
  
  report_view <- map_reports_sarek(syn_out, project)
  view_id <- report_view$properties$id
  tb <- .syn$tableQuery(glue::glue("SELECT id,name,parentId,resourceType,assay,fileFormat,workflow,workflowLink FROM {view_id}")) 
  dt <- tb$asDataFrame()
  # Map expected file suffixes to actual file format (.e.g. `out` and `log` files are really just text files)
  file_formats_map <- c("out" = "txt",     
                        "zip" = "zip",     
                        "summary" = "txt",
                        "html" = "html",    
                        "png" = "png",     
                        "metrics" = "txt", 
                        "count" = "txt",   
                        "gif" = "gif",     
                        "txt"  = "txt",     
                        "js" = "js",      
                        "css" = "css",     
                        "qual" = "txt",    
                        "pdf" = "pdf",     
                        "svg" = "svg",
                        "log" = "txt",     
                        "json" = "json")
  
  # Get parent folder name from parent id
  parent_wf <- unique(dt$parentId)
  parent_wf_name <- c()
  for(i in parent_wf) {
    suppressMessages(x <- .syn$get(i))
    parent_wf_name[i] <- x$properties$name
  }
  
  parent_wf_name[grepl("recal$", parent_wf_name)] <- "bamQC"
  parent_wf_name[parent_wf_name == "css"] <- "bamQC"
  parent_wf_name[parent_wf_name == "images_qualimapReport"] <- "bamQC"
  parent_wf_name[parent_wf_name == "raw_data_qualimapReport"] <- "bamQC"
  parent_wf_name[grepl("multiqc", parent_wf_name)] <- "MultiQC" 
  parent_wf_name[parent_wf_name == "pdf"] <- "MultiQC" 
  parent_wf_name[parent_wf_name == "svg"] <- "MultiQC" 
  parent_wf_name[parent_wf_name == "png"] <- "MultiQC" 
  
  parent_wf_name[grepl("Lane", parent_wf_name)] <- NA # disambiguate between MultiQC and bamQC later
  
  # Folder name to WK Link mapping
  # Providing specific QC workflow link allows users to visit link and read the respective file doc
  qc_workflow_map <- c(BCFToolsStats = "https://nf-co.re/sarek/2.7.1/output#bcftools-stats",
                       FastQC = "https://nf-co.re/sarek/2.7.1/output#fastqc",
                       MarkDuplicates = "https://nf-co.re/sarek/2.7.1/output#gatk-markduplicates-reports",
                       SamToolsStats = "https://nf-co.re/sarek/2.7.1/output#samtools-stats",
                       VCFTools = "https://nf-co.re/sarek/2.7.1/output#vcftools",
                       bamQC = "https://nf-co.re/sarek/2.7.1/output#bamqc",
                       MultiQC = "https://nf-co.re/sarek/2.7.1/output#multiqc") 
  
  dt$resourceType <- "workflow report"
  dt$assay <- "Whole Exome Sequencing"
  dt$workflow <- parent_wf_name[dt$parentId]
  dt$workflow[grepl("fastqc", dt$name)] <- "FastQC"
  dt$workflow[dt$name %in% c("genome_results.txt", "qualimapReport.html")] <- "bamQC"
  dt$workflowLink <- qc_workflow_map[dt$workflow]
  # Checks
  # dt[is.na(dt$workflow)]
  # dt[is.na(dt$workflowLink), ]
  dt$fileFormat <- file_formats_map[tools::file_ext(dt$name)]
  manifest <- as.data.table(dt)
  manifest <- manifest[, .(entityId = id, resourceType, assay, fileFormat, workflow, workflowLink)]
  if(!dry_run) annotate_with_manifest(manifest)
  return(manifest)
}



