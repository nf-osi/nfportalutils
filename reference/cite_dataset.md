# Generate example dataset citation

This is currently more for demo purposes, to check how well current
metadata could be formatted into citation text. Datasets where DOIs have
been minted *or* NF-OSI processed datasets within the official Portal
Collection should work well, while there are no guarantees for other
cases. Note: Internal/experimental use only, not for production use.

## Usage

``` r
cite_dataset(id, format = "Scientific Data", output = c("markdown"))
```

## Arguments

- id:

  Dataset id.

- format:

  Currently just "Scientific Data" format.

- output:

  Currently only markdown, from which other utils can be used to
  generate LaTeX or HTML.
