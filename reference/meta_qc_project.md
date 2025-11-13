# QC metadata at the project level with pass/fail result

Wrapper to go through project datasets and do revalidation in
one-stop-shop manner. This works best when datasets are directly under
"Raw Data" (the standard and most-preferred organization) *or* at most
one additional level of nesting. See
https://help.nf.synapse.org/NFdocs/how-to-organize-data. For selective
validation or more complicated structures, look at `meta_qc_dataset` to
do manual or interactive dataset-by-dataset validation.

## Usage

``` r
meta_qc_project(project_id, result_file = NULL, ...)
```

## Arguments

- project_id:

  Synapse project id.

- result_file:

  If not NULL, *also* write to output to `.csv` file.

- ...:

  Params passed to `meta_qc_dataset`.

## Value

A table of with rows for the datasets QC'd, with dataset id, name,
TRUE/FALSE pass result, and summary; otherwise `NA`.
