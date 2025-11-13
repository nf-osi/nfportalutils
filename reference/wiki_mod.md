# Add markup to a project wiki

Add markup to an **existing** project wiki, e.g. regular markdown, a
widget, or other Synapse wiki compatible content. Errors will be
encountered if one tries to modify a project wiki that does not exist.

## Usage

``` r
wiki_mod(
  content,
  project_id,
  subpage = NULL,
  where = c("top", "bottom"),
  dry_run = TRUE
)
```

## Arguments

- content:

  Markdown or other markup compatible with Synapse wikis.

- project_id:

  ID of the owner Synapse project.

- subpage:

  If given a character name, will add to a new subpage of that name. If
  `NULL`, contents will be added to the main page.

- where:

  Where to add markup on page, "top" or "bottom" (defaults to "top").
  Only used if adding to main page, which may already have content.

- dry_run:

  Whether to return a wiki object only without actually performing
  update.
