# Example usage of generate_nfcore_manifest function
# This script demonstrates how to use the new function to generate nf-core samplesheets

library(nfportalutils)

# First, login to Synapse
syn_login()

# Example 1: Generate rnaseq samplesheet
# This will create a samplesheet with columns: sample, fastq_1, fastq_2, strandedness
rnaseq_manifest <- generate_nfcore_manifest(
  dataset_id = "syn12345678",  # Replace with actual Synapse dataset ID
  workflow_name = "rnaseq",
  synstage = TRUE,  # Use Synapse URIs (syn://syn12345)
  strandedness = "auto"
)

# Save to file with local file paths
generate_nfcore_manifest(
  dataset_id = "syn12345678",
  workflow_name = "rnaseq", 
  synstage = FALSE,  # Use file names instead of Synapse URIs
  output_file = "rnaseq_samplesheet.csv",
  strandedness = "reverse"
)

# Example 2: Generate sarek samplesheet  
# This will create a samplesheet with columns: subject, sample, lane, fastq_1, fastq_2, bam, umi, type
# Sample type is automatically inferred from specimenID (e.g., "Skin" = normal, "T1" = tumor)
# NOTE: A warning will be displayed that sarek hasn't been fully tested yet
sarek_manifest <- generate_nfcore_manifest(
  dataset_id = "syn12345678",  # Replace with actual Synapse dataset ID
  workflow_name = "sarek",
  synstage = TRUE  # Use Synapse URIs (syn://syn12345)
)

# Save to file with Synapse URIs
generate_nfcore_manifest(
  dataset_id = "syn12345678",
  workflow_name = "sarek",
  synstage = TRUE, 
  output_file = "sarek_samplesheet.csv"
)

# The generated samplesheets can then be used with nf-core pipelines:
# nextflow run nf-core/rnaseq -r 3.21.0 -profile docker --input rnaseq_samplesheet.csv --outdir results
# nextflow run nf-core/sarek -r 3.6.0 -profile docker --input sarek_samplesheet.csv --outdir results
