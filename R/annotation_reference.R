#' Match file format
#'
#' @param filename Filename.
#' @keywords internal
asFileFormat <- function(filename) {


  if(grepl("[.]r$", filename)) return("R script")
  if(grepl("[.]Rscript$", filename)) return("R script")
  if(grepl("[.]RData$", filename)) return("RData")
  if(grepl("[.]rds$", filename)) return("rds")
  if(grepl("[.]txt$", filename)) return("txt")
  if(grepl("[.]txt.summary$", filename)) return("txt")
  if(grepl("[.]tsv$", filename)) return("tsv")
  if(grepl("[.]csv$", filename)) return("csv")
  if(grepl("[.]cram$", filename)) return("cram")
  if(grepl("[.]crai$", filename)) return("crai")
  if(grepl("[.]vcf.gz$", filename)) return("vcf")
  if(grepl("[.]tbi$", filename)) return("tbi")
  if(grepl("[.]fq.gz$", filename)) return("fastq")
  if(grepl("[.]bam$", filename)) return("bam")
  if(grepl("[.]csi$", filename)) return("csi")
  if(grepl("[.]bigWig", filename)) return("bigwig")
  if(grepl("[.]bed$", filename)) return("bed")
  if(grepl("[.]bed.gz$", filename)) return("bed")
  if(grepl("[.]csv$", filename)) return("csv")
  if(grepl("[.]png$", filename)) return("png")
  if(grepl("[.]svg$", filename)) return("svg")
  if(grepl("[.]jpg$", filename)) return("jpg")
  if(grepl("[.]gif$", filename)) return("gif")
  if(grepl("[.]html$", filename)) return("html")
  if(grepl("[.]js$", filename)) return("js")
  if(grepl("[.]json$", filename)) return("json")
  if(grepl("[.]yml$", filename)) return("yaml")
  if(grepl("[.]yaml$", filename)) return("yaml")
  if(grepl("[.]ctab$", filename)) return("ctab")
  if(grepl("[.]tab$", filename)) return("tab")
  if(grepl("[.]gtf$", filename)) return("gtf")
  if(grepl("[.]css$", filename)) return("css")
  if(grepl("[.]flagstat$", filename)) return("flagstat")
  if(grepl("[.]idxstats$", filename)) return("idxstats")
  if(grepl("[.]stats$", filename)) return("stats")
  if(grepl("[.]out$", filename)) return("out")
  if(grepl("[.]zip$", filename)) return("zip")
  if(grepl("[.]fna.gz$", filename)) return("fna")
  if(grepl("[.]fld.gz$", filename)) return("")
  if(grepl("[.]expected_bias.gz$", filename)) return("")
  if(grepl("[.]observed_bias.*.gz$", filename)) return("")
  return(NA_character_)
}

#' Match data type
#'
#' @param filename Filename.
#' @keywords internal
asDataType <- function(filename) {

  # genomics
  if(grepl("[.]cram$", filename)) return("AlignedReads")
  if(grepl("[.]crai$", filename)) return("dataIndex")
  if(grepl("[.]vcf.gz$", filename)) return("genomicVariants")
  if(grepl("[.]tbi$", filename)) return("dataIndex")
  if(grepl("[.]fq.gz$", filename)) return("geneExpression")
  if(grepl("[.]ctab$", filename)) return("geneExpression")
  if(grepl("[.]sam$", filename)) return("AlignedReads")
  if(grepl("[.]bam$", filename)) return("AlignedReads")
  if(grepl("[.]bai$", filename)) return("dataIndex")
  if(grepl("[.]csi$", filename)) return("dataIndex")
  if(grepl("[.]bed.gz$", filename)) return("genomicFeatures")
  if(grepl("[.]bigwig$", filename)) return("genomicFeatures")
  if(grepl("[.]fna.gz$", filename)) return("referenceData")
  # if(grepl("[.]ballgown$", filename)) return("--")
  if(grepl("[.]gene.abundance.txt$", filename)) return("geneExpression")
  if(grepl("[.]gtf$", filename)) return("genomicFeatures")

  # image
  if(grepl("[.]png$", filename)) return("image")
  if(grepl("[.]svg$", filename)) return("image")
  if(grepl("[.]jpg$", filename)) return("image")
  if(grepl("[.]gif$", filename)) return("image")

  # metadata
  if(grepl("synapse_storage_manifest$", filename)) return("metadata")
  if(grepl("[.]out$", filename)) return("metadata")
  if(grepl("[.]log$", filename)) return("metadata")
  if(grepl("[.]out.tab$", filename)) return("metadata")
  if(grepl("[.]metrics$", filename)) return("metadata")
  if(grepl("[.]summary$", filename)) return("metadata")

  return(NA_character_)

}

#' Match expression unit
#'
#' @param filename Filename.
#' @keywords internal
asExpressionUnit <- function(filename) {
  if(grepl("gene_counts_length_scaled", filename)) return("Counts")
  if(grepl("gene_counts_scaled", filename)) return("Counts")
  if(grepl("gene_counts", filename)) return("Counts")
  if(grepl("transcript_counts", filename)) return("Counts")
  if(grepl("transcript_tpm", filename)) return("TPM")
  if(grepl("gene_tpm", filename)) return("TPM")
  return(NA_character_)
}
