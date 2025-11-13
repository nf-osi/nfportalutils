# Generic template for clinical data file

Make meta file to describe one of the clinical data files (e.g. SAMPLE,
PATIENT). Adapted from
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/make_meta.R#L65

## Usage

``` r
make_meta_clinical_generic(
  cancer_study_identifier,
  genetic_alteration_type,
  datatype,
  data_filename
)
```

## Arguments

- cancer_study_identifier:

  The study identifier.

- genetic_alteration_type:

  The cBioPortal generic alteration type.

- datatype:

  The cBioPortal data type of `data_filename`.

- data_filename:

  Name of the data file that this meta file describes.
