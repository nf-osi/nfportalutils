# Export and add clinical data to cBioPortal dataset

This should be run in an existing dataset package root.

## Usage

``` r
cbp_add_clinical(clinical_data, ref_map, clinical_type = NULL, verbose = TRUE)
```

## Arguments

- clinical_data:

  Clinical table query.

- ref_map:

  YAML file specifying the mapping of (NF) clinical metadata to
  cBioPortal model. See details.

- clinical_type:

  (Optional) Add as "SAMPLE" or "PATIENT" clinical data. If not given,
  will infer which files need to be written.

- verbose:

  Whether to provide informative messages throughout.

## Details

Clinical data are mapped and exported according to a reference mapping.
Also reformatting of `PATIENT_ID`, `SAMPLE_ID` to contain only letters,
numbers, points, underscores, hyphens; in Nextflow processing any spaces
gets replaced with underscores so that's the default here. Does *not*
check for missing samples, as final validation via cBioPortal tool is
still expected for that.
