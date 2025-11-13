# Annotate processed aligned reads

Given a manifest, annotate data as aligned reads data. Returns a
"partial" manifest, which can be adjusted as needed, e.g. add additional
comments or batch info.

## Usage

``` r
annotate_aligned_reads(
  metadata,
  workflow_link,
  genomic_reference = "GRCh38",
  verbose = TRUE
)
```

## Arguments

- metadata:

  Metadata table to build upon.

- workflow_link:

  Workflow link to most specific part of workflow generating these data.

- genomic_reference:

  For aligned reads, genomic reference meta should be present; defaults
  to GRCh38.

- verbose:

  Give verbose reports for what's happening.
