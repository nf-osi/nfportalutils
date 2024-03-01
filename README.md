
# nfportalutils

<!-- badges: start -->
![R-CMD-check](https://github.com/nf-osi/nfportalutils/workflows/R-CMD-check/badge.svg?branch=develop)
<!-- badges: end -->

The goal of `nfportalutils` is to provide convenience functions for project and (meta)data management in the NF-OSI data portal scope. 
Currently, `develop` branch is default so package install and docs refer to code in this branch.  

> [!WARNING] 
> For the last relatively stable version of `nfportalutils`, please install at https://github.com/nf-osi/nfportalutils/releases/tag/v0.9500-presynapser.
>
> Currently, the package is in a refactoring period where usage is complex because of the coexistence of both `synapser` and separate `synapseclient` import.
> This will be updated when everything is 100% refactored to `synapser`. 

## Docs

:point_right: [Package documentation!](https://nf-osi.github.io/nfportalutils/)  

## Installation

You should first install `synapser` following the instructions [here](https://github.com/Sage-Bionetworks/synapser?tab=readme-ov-file#installation).

Then you can install `nfportalutils` with:
``` r
remotes::install_github("nf-osi/nfportalutils")
```


## Additional Notes for Users

- View function reference on docs site at [Reference](https://nf-osi.github.io/nfportalutils/reference/index.html). 
- An alternative to viewing vignettes as Articles on the [docs site](https://nf-osi.github.io/nfportalutils/index.html) is to download them with pkg install and load with e.g. `vignette("annotate-nf-processed-data", package = "nfportalutils")` to view.

## Additional Notes for Contributors 

### Contrib workflow
- Branch off `develop` and make changes
- Run `devtools::check(vignettes = FALSE)` early and often, and definitely before submitting a PR
- Make a pull request to `develop`; this will run `R-CMD-CHECK` and `pkgdown`
- Request a reviewer if both checks pass
- Reviewer requests changes or merges

### Local development tips
- Some vignettes need to be [precomputed](https://ropensci.org/blog/2019/12/08/precompute-vignettes/). 
- Again, run `devtools::check(vignettes = FALSE)` early and often.
- At minimal, address any `ERRORS` and `WARNINGS`.
- Yes, we do have a lot of `NOTES` that need to be resolved. 
- For custom indexing after adding/updating functions, edit the `_pkgdown.yml`.
- Preview the pkg docs site locally with `pkgdown::build_site()`.


## Code of Conduct
  
Please note that the nfportalutils project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
