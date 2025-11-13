# Check that in valid cBioPortal study dataset root

The `cbp_add*` functions need to be run while in the study package root.
This checks in valid study directory and returns the `cancer_study_id`.

## Usage

``` r
check_cbp_study_id()
```

## Value

`cancer_study_id` for the current cBioPortal cancer study.
