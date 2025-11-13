# Make meta file for maf

Reused from
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/make_meta.R#L157

## Usage

``` r
make_meta_maf(
  cancer_study_identifier,
  data_filename = "data_mutations.txt",
  publish_dir = ".",
  write = TRUE,
  verbose = TRUE
)
```

## Arguments

- cancer_study_identifier:

  The study identifier.

- data_filename:

  Name of the data file. Defaults to "data_mutations.txt".

- publish_dir:

  Directory path to write to, defaults to current.

- write:

  Whether to write the meta file for the data file.

- verbose:

  Report where file has been written.
