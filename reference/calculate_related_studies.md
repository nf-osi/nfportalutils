# Calculate and add related studies to study table

Processes study summary text to identify clusters of related studies.
Calculates tf-idf values for 1 and 2 length ngrams, and clusters studies
using the ward.D clustering method. Adds results as annotations to the
studies.

## Usage

``` r
calculate_related_studies(
  study_table_id,
  n_clust = NULL,
  n_k = NULL,
  dry_run = TRUE
)
```

## Arguments

- study_table_id:

  The synapse id of the portal study table. Must have write access.

- n_clust:

  Target number of clusters to generate using hierarchical clustering.
  In practice, the number of total summaries divided by 3 is a good
  starting point (100 studies = 33 clusters). If given `n_k` is ignored.

- n_k:

  Generate target number of most closely related studies using
  k-nearest-neighbors instead; since the number of desired related
  studies is specified, this may be preferable over using `n_clust`,
  which gives variable number of related studies because clusters vary
  in size. Ignored if `n_clust` is already given.

- dry_run:

  Default = TRUE. Skips annotating the studies and instead prints study
  tibble.

## Value

If dry_run == T, returns study tibble and skips upload.

## Examples

``` r
if (FALSE) { # \dontrun{
result1  <- calculate_related_studies(study_table_id = "syn16787123",
                           n_clust = 40,
                           dry_run = T)
result2  <- calculate_related_studies(study_table_id = "syn16787123",
                           n_k = 4,
                           dry_run = T)
x <- lapply(result1$relatedStudies, jsonlite::fromJSON)
y <- lapply(result2$relatedStudies, jsonlite::fromJSON)
# Compare
mapply(function(x, y) sum(y %in% x), x, y)
} # }
```
