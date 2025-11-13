# Get cBioPortal clinical file name based on clinical data type

This is called from the wrapper `write_cbio_clinical`. Adapted from
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_clinical.R#L411.
Note that of the clinical file types, the PATIENT type can actually be
optional, and because we (NF) currently don't use the TIMELINE type, the
options have been simplified.

## Usage

``` r
get_cbio_filename(clinical_type = c("SAMPLE", "PATIENT"))
```

## Arguments

- clinical_type:

  String representing cBioPortal clinical data type.

## Value

string
