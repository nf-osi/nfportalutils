# Infer data type of a dataset folder

Infer the data type by checking the first few files. TODO: Check
`dataType` instead of Component and derive Component because some older
files does not have Component explicitly.

## Usage

``` r
infer_data_type(dataset_id)
```

## Arguments

- dataset_id:

  Optional, if given this fills out manifest for existing dataset
  instead of generating a blank manifest.

## Value

List of structure `list(result = result, notes = notes)`, where `result`
can be `NA`.
