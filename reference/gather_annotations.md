# Internal helper for gathering annotations into a table using the REST API

This is an internal implementation that works directly with the platform
service JSON to afford more low-level control and avoid Python-R object
conversion differences between reticulate versions.

## Usage

``` r
gather_annotations(ids, list_sep = ", ")
```

## Arguments

- ids:

  One or more ids.

- list_sep:

  List separator for list annotations.
