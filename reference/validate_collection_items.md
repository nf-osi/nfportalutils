# Validate metadata of items in a collection

This is usually used with a dataset collection. Items in a collection
inherit the schema binding from the collection, so this function
validates each item's annotations against the inherited schema using
Synapse's native validation services.

## Usage

``` r
validate_collection_items(collection_id, mc.cores = NULL)

validate_dataset_items(collection_id, mc.cores = NULL)
```

## Arguments

- collection_id:

  Collection id.

- mc.cores:

  Number of cores to use for parallel validation. Defaults to
  `min(parallel::detectCores() / 2, 4)` to be conservative. Set to 1 to
  disable parallel processing.

## Value

A list with `validation_error` component containing any validation
errors found.
