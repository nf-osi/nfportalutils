# Annotate quantified expression output

Given a manifest, annotate data as level 3 processed expression data,
using defaults from star_salmon processing. Returns a "partial"
manifest, which can be adjusted as needed, e.g. add additional comments
or batch info.

## Usage

``` r
annotate_quantified_expression(metadata, workflow_link, verbose = TRUE)
```

## Arguments

- metadata:

  Metadata table to build upon.

- workflow_link:

  Workflow link to most specific part of workflow generating these data.

- verbose:

  Give verbose reports for what's happening.
