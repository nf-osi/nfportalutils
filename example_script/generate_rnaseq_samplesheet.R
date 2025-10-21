## Generate RNA-seq samplesheet for nf-core/rnaseq pipeline
## This script creates a samplesheet CSV file from a Synapse dataset

library(nfportalutils)
library(data.table)

## Input dataset ID
INPUT_DATASET <- "syn70366294"
OUTPUT_FILE <- "samplesheet.csv"

## Login to Synapse
nfportalutils::syn_login()

## Query dataset
dt <- table_query(INPUT_DATASET)
dt <- as.data.table(dt)

## Create samplesheet with required columns:
# 1st column = sample
# 2nd column = single_end # if dt$runType == "pairedEnd" then 0 else 1
# 3rd column = fastq_1 # determine pairs via dt$specimenID, then assign first fastq based on dt$readPair == 1
# 4th column = fastq_2 # dt$readPair == 2
# 5th column = strandedness # default auto

samplesheet <- dt[, .(
  sample = specimenID,
  single_end = ifelse(runType == "pairedEnd", 0, 1),
  fastq_1 = id[readPair == 1],
  fastq_2 = id[readPair == 2],
  strandedness = "auto"
), by = specimenID][, specimenID := NULL]

## Remove duplicates
samplesheet <- unique(samplesheet)

## Remove any samples that don't have both read pairs (if paired-end)
samplesheet <- samplesheet[!(single_end == 0 & (is.na(fastq_1) | is.na(fastq_2)))]

## For single-end data, set fastq_2 to empty string
samplesheet[single_end == 1, fastq_2 := ""]

## Write samplesheet to CSV
fwrite(samplesheet, OUTPUT_FILE, quote = FALSE)
message("Samplesheet written to: ", OUTPUT_FILE)
message("Total samples: ", nrow(samplesheet))
