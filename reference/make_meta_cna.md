# Make meta file for cBioPortal copy number alteration data

Currently assumes seg data and should be extended later.

## Usage

``` r
make_meta_cna(
  cancer_study_identifier,
  data_filename = "data_cna.seg",
  reference_genome_id = "hg19",
  publish_dir = ".",
  write = TRUE,
  verbose = TRUE
)
```

## Details

See https://docs.cbioportal.org/file-formats/#segmented-data
