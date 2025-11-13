# Infer data type of a dataset folder

Infer the data type by checking the first few files.

## Usage

``` r
infer_data_type(dataset_id)
```

## Arguments

- dataset_id:

  Synapse ID of the dataset to query.

## Value

- data_type:

  Inferred data type of dataset_id. NA if unable to infer data type or
  conflicting data types observed.
