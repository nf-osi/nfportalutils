# Validate dataset folder

Convenience function to validate a dataset folder. This will use the
schema bound to the dataset for validation. If no schema is bound, this
will give an error. Files inherit the schema binding from the parent
folder, so only the folder binding is checked.

## Usage

``` r
validate_dataset_folder(id, fileview, mc.cores = NULL)
```

## Arguments

- id:

  Synapse id of a dataset folder

- fileview:

  Fileview to use for resolving folder items (required). Must have the
  `path` column enabled to support recursive file discovery.

- mc.cores:

  Number of cores to use for parallel validation. Defaults to
  `min(parallel::detectCores() / 2, 4)` to be conservative. Set to 1 to
  disable parallel processing.

## Value

A list with `validation_error` component containing any validation
errors found for files.
