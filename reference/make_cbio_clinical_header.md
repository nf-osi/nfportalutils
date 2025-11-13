# Make header for cBioPortal clinical data file

This is called from the wrapper `write_cbio_clinical`. Adapted from
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_clinical.R#L396.
Needs a data table of clinical data and a reference providing `label`,
`description`, and `data_type`.

## Usage

``` r
make_cbio_clinical_header(df, mapping)
```

## Arguments

- df:

  A `data.frame` representing clinical dataset to publicize.

- mapping:

  A reference table providing `label`, `description`, and `data_type`
  for each `source` attribute in `df`.
