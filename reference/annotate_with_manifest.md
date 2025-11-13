# Set annotations from a manifest

The [Synapse
docs](https://help.synapse.org/docs/Managing-Custom-Metadata-at-Scale.2004254976.html)
suggest doing batch annotations through a fileview. However, it is often
simpler to modify or set new annotations directly given a table of just
the entities (rows) and props (cols) we want. This is like how schematic
works, except without any validation (so works best for power-users who
know the data model well). Some desired defaults are taken into account,
such as not submitting key-values with `NA` and empty strings.

## Usage

``` r
annotate_with_manifest(
  manifest,
  ignore_na = TRUE,
  ignore_blank = TRUE,
  verbose = FALSE
)
```

## Arguments

- manifest:

  A `data.frame` representing a manifest. Needs to contain `entityId`
  (if parsed from a standard manifest.csv, the df should already contain
  `entityId`).

- ignore_na:

  Whether to ignore annotations that are `NA`; default TRUE.

- ignore_blank:

  Whether to ignore annotations that are that empty strings; default
  TRUE.

- verbose:

  Be chatty, default FALSE.
