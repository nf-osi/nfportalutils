# Generic table query

Retrieve selected data from a Synapse table.

## Usage

``` r
table_query(table_id, columns = "*", includeRowIdAndRowVersion = F)
```

## Arguments

- table_id:

  Synapse table id.

- columns:

  A character vector of selected columns (which often correspond to
  annotations, but not always). If not given, will select all columns.

- includeRowIdAndRowVersion:

  Whether to include the row id and etag, defaults to `FALSE`. If the
  use case is to update rows in the table (rather than just to retrieve
  information for viewing, use `TRUE`).

## Value

A tibble.
