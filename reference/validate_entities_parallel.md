# Validate multiple entities in parallel

Helper function to validate multiple entities against their bound
schemas in parallel.

## Usage

``` r
validate_entities_parallel(entity_ids, mc.cores = NULL)
```

## Arguments

- entity_ids:

  Vector of entity IDs to validate.

- mc.cores:

  Number of cores to use for parallel validation. Defaults to
  `min(parallel::detectCores() / 2, 4)` to be conservative. Set to 1 to
  disable parallel processing.

## Value

Named list of validation results, where names are entity IDs.
