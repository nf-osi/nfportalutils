# Export specific type of clinical data

Export specific type of clinical data

## Usage

``` r
as_clinical_file_type(
  df,
  clinical_type = c("SAMPLE", "PATIENT"),
  m,
  na_recode = getOption("nfportalutils.na_recode"),
  publish_dir = ".",
  verbose = TRUE
)
```

## Arguments

- df:

  A `data.frame` representing clinical dataset to publicize.

- clinical_type:

  `SAMPLE` or `PATIENT`

- m:

  A reference mapping object. See `use_ref_map`.

- na_recode:

  Values considered NA that will be blank strings in cBioPortal's
  preferred format.

- publish_dir:

  Directory path to write to, defaults to current.

- verbose:

  Report where file has been written.
