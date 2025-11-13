# Helper function to check urls

Check whether URL(s) return a HTTP error, indicating a broken link. Note
that this uses `curl` under the hood, which may give timeout errors and
therefore false positives for some links that are valid but take too
long to resolve.

## Usage

``` r
bad_url(url)
```

## Arguments

- url:

  A character vector of one or more URLs.

## Value

Result vector of same size with values "bad" or "ok".
