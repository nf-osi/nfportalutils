# Make meta file for cBioPortal expression data

https://docs.cbioportal.org/file-formats/#expression-data

## Usage

``` r
make_meta_expression(
  cancer_study_identifier,
  type = "raw",
  data_filename = glue::glue("data_expression_{type}.txt"),
  publish_dir = ".",
  write = TRUE,
  verbose = TRUE
)
```
