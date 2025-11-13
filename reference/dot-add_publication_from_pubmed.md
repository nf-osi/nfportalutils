# Higher-level fun to generate `add_publication_from_pubmed` util for one-off usage (default) or optimized for batch processing.

Higher-level fun to generate `add_publication_from_pubmed` util for
one-off usage (default) or optimized for batch processing.

## Usage

``` r
.add_publication_from_pubmed(batch = 0L, cache = batch)
```

## Arguments

- batch:

  If a non-zero batch size, turns on batch mode; defaults to no-batch.

- cache:

  Whether to cache some results, which is default if `batch`.
