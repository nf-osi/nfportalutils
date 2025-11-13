# Parse nextflow samplesheet for sample inputs

Samplesheets are used in rnaseq pipelines, defined here:
https://nf-co.re/rnaseq/usage#full-samplesheet. After the pipeline is
run, it should be found in an output folder called `pipeline_info`. This
is a simple helper to get a mapping of sample ids to input files (either
one-to-many or one-to-one) as a table.

## Usage

``` r
map_sample_input_ss(
  samplesheet,
  parse_fun = function(x) gsub("_T[0-9]+$", "", x)
)
```

## Arguments

- samplesheet:

  A local file or syn id of samplesheet.

- parse_fun:

  Function implementing how to parse samples in samplesheet.
