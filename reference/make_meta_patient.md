# Make patient meta file

Adapted from
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L101

## Usage

``` r
make_meta_patient(
  cancer_study_identifier,
  data_filename = "data_clinical_patient.txt",
  write = TRUE,
  publish_dir = ".",
  verbose = TRUE
)
```

## Arguments

- cancer_study_identifier:

  The study identifier.

- data_filename:

  Name of the data file that this meta file describes.

- write:

  Whether to write the meta file for the clinical data file.

- publish_dir:

  Directory path to write to, defaults to current.

- verbose:

  Report where file has been written.
