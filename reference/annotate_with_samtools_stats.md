# Make annotations from samtools stats

Extracts a subset of [samtools
stats](http://www.htslib.org/doc/samtools-stats.md) Regarding the
selection of stats, see the Genomic Data Commons (GDC) model for
[AlignedReads](https://docs.gdc.cancer.gov/Data_Dictionary/viewer/#?view=table-definition-view&id=aligned_reads)

## Usage

``` r
annotate_with_samtools_stats(meta, samtools_stats_file = NULL, verbose = TRUE)
```

## Arguments

- meta:

  Data to which tool stats will be added as additional meta.

- samtools_stats_file:

  Path to file/syn id of file with samtools stats produced by the
  workflow.

- verbose:

  Whether to output detailed debugging messages.
