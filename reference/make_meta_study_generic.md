# Template for meta study file

Adapted from
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L90
Low-level internal function for the tedious templating.

## Usage

``` r
make_meta_study_generic(
  cancer_study_identifier,
  type_of_cancer,
  name,
  description,
  citation = NULL,
  pmid = NULL,
  groups = NULL,
  short_name = NULL,
  add_global_case_list = TRUE
)
```
