# Create NF Data Curator App subpage

Convenience method to create a subpage with the default buttons for the
annotation app and docs. This is a highly specific method and expected
to have a limited lifespan.

## Usage

``` r
data_curator_app_subpage(project_id, dry_run = TRUE)
```

## Arguments

- project_id:

  ID of the owner Synapse project.

- dry_run:

  Whether to return a wiki object only without actually performing
  update.
