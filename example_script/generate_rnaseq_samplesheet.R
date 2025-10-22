## Generate RNA-seq samplesheet for nf-core/rnaseq pipeline
## This script creates a samplesheet CSV file from a Synapse dataset

library(nfportalutils)
library(data.table)

## Input dataset ID
INPUT_DATASET <- "syn70366294"
OUTPUT_FILE <- "samplesheet.csv"

## Login to Synapse
# nfportalutils::syn_login()

## Query dataset
dt <- table_query(INPUT_DATASET)
dt <- as.data.table(dt)

## Or use test_dt.csv instead
## dt <- fread("test_dt.csv")

## Create samplesheet with required columns per nf-core/rnaseq schema:
# 1st column = sample (unique identifier without spaces)
# 2nd column = fastq_1 (syn:// URI to first read file)
# 3rd column = fastq_2 (syn:// URI to second read file, empty for single-end)
# 4th column = strandedness (forward, reverse, unstranded, or auto)

samplesheet <- dt[, .(
  sample = unique(specimenID),
  fastq_1 = paste0("syn://", id[readPair == 1]),
  fastq_2 = paste0("syn://", id[readPair == 2]),
  strandedness = "auto",
  runType = unique(runType)
), by = specimenID][, specimenID := NULL]

## Remove duplicates
samplesheet <- unique(samplesheet)

## For paired-end data, remove any samples missing either read pair
samplesheet <- samplesheet[!(runType == "pairedEnd" & (is.na(fastq_1) | is.na(fastq_2)))]

## For single-end data, set fastq_2 to empty string
samplesheet[runType != "pairedEnd", fastq_2 := ""]

## Remove the runType helper column
samplesheet[, runType := NULL]

## Write samplesheet to CSV
fwrite(samplesheet, OUTPUT_FILE, quote = FALSE)
message("Samplesheet written to: ", OUTPUT_FILE)
message("Total samples: ", nrow(samplesheet))
