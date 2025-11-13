# Calculate study distance based on summary text

There are different measures of similarity; this gives cosine similarity
based on summary text, which is then converted to a distance matrix. In
the future, other methods may be used for comparison or in ensemble.

## Usage

``` r
calc_study_dist_dtm(studies)
```

## Arguments

- studies:

  A `data.frame` where each row is a "document"; should have `summary`
  and `studyId`.
