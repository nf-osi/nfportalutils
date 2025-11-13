# Generate nf-core samplesheet manifest

Generate a samplesheet manifest for nf-core workflows (rnaseq or sarek)
from Synapse dataset metadata. The function queries the dataset metadata
table and reformats it into the appropriate samplesheet format where
each row represents a biological sample that may have multiple
associated files.

## Usage

``` r
generate_nfcore_manifest(
  dataset_id,
  workflow_name,
  synstage = FALSE,
  output_file = NULL,
  strandedness = "auto"
)
```

## Arguments

- dataset_id:

  Synapse ID of the dataset to query

- workflow_name:

  Name of the nf-core workflow ("rnaseq" or "sarek")

- synstage:

  Logical indicating whether to use Synapse staging for file paths

- output_file:

  Optional path to save the samplesheet CSV file. If NULL, returns the
  data frame.

- strandedness:

  For rnaseq workflow, strandedness setting ("forward", "reverse",
  "unstranded", or "auto"). Defaults to "auto".

## Value

A data frame with the samplesheet format appropriate for the specified
workflow

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate rnaseq samplesheet
rnaseq_manifest <- generate_nfcore_manifest(
  dataset_id = "syn12345678",
  workflow_name = "rnaseq",
  synstage = TRUE
)

# Generate sarek samplesheet
sarek_manifest <- generate_nfcore_manifest(
  dataset_id = "syn12345678", 
  workflow_name = "sarek",
  synstage = FALSE
)
} # }
```
