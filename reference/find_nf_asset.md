# Find a standard nextflow workflow output asset

Note that samplesheets became part of the output only for newer versions
of nf-core/rna-seq; older runs may not find samplesheets. Paths default
to known working paths corresponding to the latest major workflow
version, but this may change and may need to be updated as part of util
maintenance.

## Usage

``` r
find_nf_asset(
  syn_out,
  asset = c("software_versions", "multiqc_report", "samplesheet", "samtools_stats"),
  workflow = "nf-rnaseq"
)
```

## Arguments

- syn_out:

  Id of top-level folder that corresponds to `publishDir` in a nextflow
  workflow.

- asset:

  Name of asset to find.

- workflow:

  Specify workflow, "nf-rnaseq" or "nf-sarek"; defaults to "nf-rnaseq".

## Value

Id of samplesheet.
