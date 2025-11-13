# Annotate nextflow workflow outputs

High-level wrapper that automatically generates annotation manifests for
nextflow workflow outputs. This function handles the complete workflow:
extracting version info, parsing inputs/outputs, and generating
manifests with proper metadata.

## Usage

``` r
annotate_nf_workflow(
  publish_dir,
  fileview,
  workflow = c("nf-rnaseq", "nf-sarek"),
  samplesheet = NULL,
  output_types = NULL,
  parse_fun = function(x) gsub("_T[0-9]+$", "", x),
  use_sample_as_specimen_id = FALSE,
  syn_out = NULL
)
```

## Arguments

- publish_dir:

  Synapse ID of the top-level workflow output directory (publishDir).
  This directory should contain the `pipeline_info` folder with workflow
  metadata.

- fileview:

  Synapse ID of a fileview that includes the workflow output files. The
  fileview must have the `path` column enabled to support output
  discovery.

- workflow:

  Type of workflow: "nf-rnaseq" or "nf-sarek".

- samplesheet:

  Synapse ID or local path to the samplesheet CSV file. If NULL
  (default), will automatically search for
  `pipeline_info/samplesheet.valid.csv`. Only specify this if using a
  custom samplesheet location or a manually corrected version.

- output_types:

  Optional character vector specifying which output types to process. If
  NULL, processes all available output types for the workflow. For
  nf-rnaseq: "STAR and Salmon", "featureCounts", "SAMtools". For
  nf-sarek: "CNVkit", "DeepVariant", "Strelka2", "Mutect2", "FreeBayes".

- parse_fun:

  Optional function to parse sample names from samplesheet. Defaults to
  removing `"_T[0-9]+$"` suffix (removes trailing technical replicate
  numbers like \_T1, \_T2, \_T10, etc.).

- use_sample_as_specimen_id:

  If TRUE, sets specimenID from the sample column (parsed from directory
  structure/filenames) instead of inheriting from input files. Useful
  when directory structure provides more granular/accurate specimen
  identifiers than input file annotations. Defaults to FALSE (inherits
  specimenID from input files).

- syn_out:

  Synapse ID of the specific output folder to annotate. For nf-rnaseq,
  this is typically the `star_salmon` folder within publish_dir. For
  nf-sarek, this is typically the `variant_calling` folder. If NULL
  (default), will use publish_dir as the output folder for standard
  workflow organization.

## Value

List with three elements:

- `manifests`: Named list of data.tables, one manifest per output type

- `sample_io`: data.table linking inputs to outputs for provenance

- `workflow_info`: List with workflow name and version

## Examples

``` r
if (FALSE) { # \dontrun{
# Simplest usage - auto-detects samplesheet and uses standard folders
result <- annotate_nf_workflow(
  publish_dir = "syn51476810",
  fileview = "syn11601481",
  workflow = "nf-rnaseq"
)

# With custom samplesheet (e.g., manually corrected)
result <- annotate_nf_workflow(
  publish_dir = "syn51476810",
  fileview = "syn11601481",
  workflow = "nf-rnaseq",
  samplesheet = "~/corrected_samplesheet.csv"
)

# Advanced: Custom output folder structure
result <- annotate_nf_workflow(
  publish_dir = "syn51476810",
  fileview = "syn11601481",
  workflow = "nf-rnaseq",
  syn_out = "syn51476811"  # Custom star_salmon folder
)

# Use sample names from directory structure as specimenID
# (useful when input files have specimenID at higher level than samplesheet)
result <- annotate_nf_workflow(
  publish_dir = "syn51476810",
  fileview = "syn11601481",
  workflow = "nf-rnaseq",
  use_sample_as_specimen_id = TRUE
)
} # }
```
