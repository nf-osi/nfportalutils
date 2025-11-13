# Metadata for processed products

Wrapper for all steps to get manifest for processed product

## Usage

``` r
processed_meta(
  input,
  output,
  workflow_link,
  use_sample_as_specimen_id = FALSE,
  publish_dir = NULL
)
```

## Arguments

- input:

  Data from `map_sample_input_ss`.

- output:

  Data from `map_sample_output_*`.

- workflow_link:

  Workflow link.

- use_sample_as_specimen_id:

  If TRUE, sets specimenID from the sample column (parsed from directory
  structure/filenames) instead of inheriting from input files.

- publish_dir:

  Top-level publish directory for finding workflow assets like SAMtools
  stats.

## Value

List `manifest` with manifests for each processed dataset, and
`sample_io` with linked inputs and outputs (should be used for
provenance annotation).
