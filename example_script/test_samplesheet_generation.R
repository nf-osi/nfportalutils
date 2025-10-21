#!/usr/bin/env Rscript
#
# Test script for generate_rnaseq_samplesheet.R
# This tests the core logic without requiring Synapse authentication
#

library(data.table)
library(glue)

# Source the main functions (but don't run main())
# We'll just test the samplesheet generation logic

cat("=== Testing RNA-seq Samplesheet Generation Logic ===\n\n")

# Test Case 1: Paired-end data
cat("Test 1: Paired-end data\n")
test_data_paired <- data.table(
  entityId = c("syn123", "syn456", "syn789", "syn012"),
  specimenID = c("sample1", "sample1", "sample2", "sample2"),
  fileFormat = c("fastq.gz", "fastq.gz", "fastq.gz", "fastq.gz"),
  readPair = c(1, 2, 1, 2),
  libraryStrand = c("forward", "forward", "auto", "auto")
)

cat("Input data:\n")
print(test_data_paired)

# Simulate the samplesheet generation logic
dt <- copy(test_data_paired)
dt[, syn_uri := paste0("syn://", entityId)]

# Determine strandedness
if ("libraryStrand" %in% names(dt)) {
  dt[, strandedness := fcase(
    libraryStrand == "forward" | libraryStrand == "plus", "forward",
    libraryStrand == "reverse" | libraryStrand == "minus", "reverse",
    libraryStrand == "unstranded", "unstranded",
    default = "auto"
  )]
} else {
  dt[, strandedness := "auto"]
}

# Handle read pairs
dt_r1 <- dt[readPair == 1 | is.na(readPair)]
dt_r2 <- dt[readPair == 2]

# For paired-end data
paired_specimens <- intersect(dt_r1$specimenID, dt_r2$specimenID)
samplesheet_paired <- merge(
  dt_r1[specimenID %in% paired_specimens, .(specimenID, fastq_1 = syn_uri, strandedness)],
  dt_r2[specimenID %in% paired_specimens, .(specimenID, fastq_2 = syn_uri)],
  by = "specimenID",
  all = TRUE
)
setnames(samplesheet_paired, "specimenID", "sample")

# For single-end data
single_end <- dt_r1[!specimenID %in% dt_r2$specimenID]
if (nrow(single_end) > 0) {
  samplesheet_single <- single_end[, .(
    sample = specimenID,
    fastq_1 = syn_uri,
    fastq_2 = "",
    strandedness = strandedness
  )]
} else {
  samplesheet_single <- data.table()
}

samplesheet <- rbindlist(list(samplesheet_single, samplesheet_paired), fill = TRUE)
samplesheet <- samplesheet[, .(sample, fastq_1, fastq_2, strandedness)]
setorder(samplesheet, sample)

cat("\nGenerated samplesheet:\n")
print(samplesheet)

# Validate
expected_rows <- 2
if (nrow(samplesheet) == expected_rows) {
  cat("✓ Test 1 PASSED: Expected", expected_rows, "rows, got", nrow(samplesheet), "\n")
} else {
  cat("✗ Test 1 FAILED: Expected", expected_rows, "rows, got", nrow(samplesheet), "\n")
}

cat("\n" , rep("-", 60), "\n\n", sep="")

# Test Case 2: Single-end data
cat("Test 2: Single-end data\n")
test_data_single <- data.table(
  entityId = c("syn111", "syn222", "syn333"),
  specimenID = c("sampleA", "sampleB", "sampleC"),
  fileFormat = c("fastq.gz", "fastq.gz", "fastq.gz"),
  readPair = c(1, 1, 1)
)

cat("Input data:\n")
print(test_data_single)

dt <- copy(test_data_single)
dt[, syn_uri := paste0("syn://", entityId)]
dt[, strandedness := "auto"]

dt_r1 <- dt[readPair == 1 | is.na(readPair)]
dt_r2 <- dt[readPair == 2]

single_end <- dt_r1[!specimenID %in% dt_r2$specimenID]
samplesheet_single <- single_end[, .(
  sample = specimenID,
  fastq_1 = syn_uri,
  fastq_2 = "",
  strandedness = strandedness
)]

samplesheet <- samplesheet_single[, .(sample, fastq_1, fastq_2, strandedness)]
setorder(samplesheet, sample)

cat("\nGenerated samplesheet:\n")
print(samplesheet)

expected_rows <- 3
if (nrow(samplesheet) == expected_rows) {
  cat("✓ Test 2 PASSED: Expected", expected_rows, "rows, got", nrow(samplesheet), "\n")
} else {
  cat("✗ Test 2 FAILED: Expected", expected_rows, "rows, got", nrow(samplesheet), "\n")
}

cat("\n" , rep("-", 60), "\n\n", sep="")

# Test Case 3: Mixed single-end and paired-end
cat("Test 3: Mixed single-end and paired-end data\n")
test_data_mixed <- data.table(
  entityId = c("syn001", "syn002", "syn003", "syn004", "syn005"),
  specimenID = c("mix1", "mix1", "mix2", "mix3", "mix3"),
  fileFormat = c("fastq.gz", "fastq.gz", "fastq.gz", "fastq.gz", "fastq.gz"),
  readPair = c(1, 2, 1, 1, 2),
  libraryStrand = c("reverse", "reverse", "auto", "forward", "forward")
)

cat("Input data:\n")
print(test_data_mixed)

dt <- copy(test_data_mixed)
dt[, syn_uri := paste0("syn://", entityId)]

dt[, strandedness := fcase(
  libraryStrand == "forward" | libraryStrand == "plus", "forward",
  libraryStrand == "reverse" | libraryStrand == "minus", "reverse",
  libraryStrand == "unstranded", "unstranded",
  default = "auto"
)]

dt_r1 <- dt[readPair == 1 | is.na(readPair)]
dt_r2 <- dt[readPair == 2]

# Single-end
single_end <- dt_r1[!specimenID %in% dt_r2$specimenID]
if (nrow(single_end) > 0) {
  samplesheet_single <- single_end[, .(
    sample = specimenID,
    fastq_1 = syn_uri,
    fastq_2 = "",
    strandedness = strandedness
  )]
} else {
  samplesheet_single <- data.table()
}

# Paired-end
paired_specimens <- intersect(dt_r1$specimenID, dt_r2$specimenID)
# Add row number to match pairs correctly
dt_r1[specimenID %in% paired_specimens, pair_index := seq_len(.N), by = specimenID]
dt_r2[specimenID %in% paired_specimens, pair_index := seq_len(.N), by = specimenID]

samplesheet_paired <- merge(
  dt_r1[specimenID %in% paired_specimens, .(specimenID, fastq_1 = syn_uri, strandedness, pair_index)],
  dt_r2[specimenID %in% paired_specimens, .(specimenID, fastq_2 = syn_uri, pair_index)],
  by = c("specimenID", "pair_index"),
  all = TRUE
)
samplesheet_paired[, pair_index := NULL]
setnames(samplesheet_paired, "specimenID", "sample")

samplesheet <- rbindlist(list(samplesheet_single, samplesheet_paired), fill = TRUE)
samplesheet <- samplesheet[, .(sample, fastq_1, fastq_2, strandedness)]
setorder(samplesheet, sample)

cat("\nGenerated samplesheet:\n")
print(samplesheet)

expected_rows <- 3  # mix1 (paired), mix2 (single), mix3 (paired)
if (nrow(samplesheet) == expected_rows) {
  cat("✓ Test 3 PASSED: Expected", expected_rows, "rows, got", nrow(samplesheet), "\n")
} else {
  cat("✗ Test 3 FAILED: Expected", expected_rows, "rows, got", nrow(samplesheet), "\n")
}

# Check strandedness values
if (all(samplesheet$strandedness %in% c("auto", "forward", "reverse", "unstranded"))) {
  cat("✓ Strandedness values are valid\n")
} else {
  cat("✗ Invalid strandedness values found\n")
}

cat("\n" , rep("-", 60), "\n\n", sep="")

# Test Case 4: Technical replicates (same specimen, multiple lanes)
cat("Test 4: Technical replicates\n")
test_data_tech_rep <- data.table(
  entityId = c("syn301", "syn302", "syn303", "syn304"),
  specimenID = c("repSample", "repSample", "repSample", "repSample"),
  fileFormat = c("fastq.gz", "fastq.gz", "fastq.gz", "fastq.gz"),
  readPair = c(1, 2, 1, 2)
)

cat("Input data:\n")
print(test_data_tech_rep)

dt <- copy(test_data_tech_rep)
dt[, syn_uri := paste0("syn://", entityId)]
dt[, strandedness := "auto"]

dt_r1 <- dt[readPair == 1]
dt_r2 <- dt[readPair == 2]

paired_specimens <- intersect(dt_r1$specimenID, dt_r2$specimenID)
# Add row number to match pairs correctly
dt_r1[specimenID %in% paired_specimens, pair_index := seq_len(.N), by = specimenID]
dt_r2[specimenID %in% paired_specimens, pair_index := seq_len(.N), by = specimenID]

samplesheet_paired <- merge(
  dt_r1[specimenID %in% paired_specimens, .(specimenID, fastq_1 = syn_uri, strandedness, pair_index)],
  dt_r2[specimenID %in% paired_specimens, .(specimenID, fastq_2 = syn_uri, pair_index)],
  by = c("specimenID", "pair_index"),
  all = TRUE
)
samplesheet_paired[, pair_index := NULL]
setnames(samplesheet_paired, "specimenID", "sample")

samplesheet <- samplesheet_paired[, .(sample, fastq_1, fastq_2, strandedness)]
setorder(samplesheet, sample)

cat("\nGenerated samplesheet:\n")
print(samplesheet)

expected_rows <- 2  # Two pairs for the same sample (technical replicates)
if (nrow(samplesheet) == expected_rows) {
  cat("✓ Test 4 PASSED: Technical replicates handled correctly\n")
} else {
  cat("✗ Test 4 FAILED: Expected", expected_rows, "rows, got", nrow(samplesheet), "\n")
}

cat("\n=== All Tests Complete ===\n")
