# Make sample meta file

Adapted from
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L109

## Usage

``` r
make_meta_sample(
  cancer_study_identifier,
  data_filename = "data_clinical_sample.txt",
  publish_dir = ".",
  write = TRUE,
  verbose = TRUE
)
```

## Arguments

- cancer_study_identifier:

  The study identifier.

- data_filename:

  Name of the data file that this meta file describes.

- publish_dir:

  Directory path to write to, defaults to current.

- write:

  Whether to write the meta file for the clinical data file.

- verbose:

  Report where file has been written.
