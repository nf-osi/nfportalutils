
# nfportalutils

<!-- badges: start -->
![R-CMD-check](https://github.com/nf-osi/nfportalutils/workflows/R-CMD-check/badge.svg?branch=develop)
<!-- badges: end -->

The goal of nfportalutils is to provide convenience functions for updating NF-OSI file metadata and portal data. 

## Installation

You can install `nfportalutils` from here:

``` r
remotes::install_github("nf-osi/nfportalutils")
```

## Package Notes

- A vignette is available for nextflow annotation utils, which is [precomputed](https://ropensci.org/blog/2019/12/08/precompute-vignettes/). 
If the vignette is downloaded, use `vignette("annotate-nf-processed-data", package = "nfportalutils")` to view.
- For development, run `devtools::check(vignettes = FALSE)` early and often.
- At minimal, address any `ERRORS` and `WARNINGS`, but we do have a lot of `NOTES` that need to be resolved... 

## Code of Conduct
  
Please note that the nfportalutils project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
