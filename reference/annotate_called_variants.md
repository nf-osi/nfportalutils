# Annotate somatic or germline variants output

Given a manifest, annotate data as variant data (`vcf` or `maf`).

## Usage

``` r
annotate_called_variants(metadata, workflow_link, verbose = TRUE)
```

## Arguments

- metadata:

  Metadata table to build upon.

- workflow_link:

  Workflow link to most specific part of workflow generating these data.

- verbose:

  Give verbose reports for what's happening.

## Details

`maf` files use the same template but with different default values. If
in the future `maf`s require a significantly different template, then
this should be factored out into a separate annotation function.
