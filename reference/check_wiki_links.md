# Check wiki links

This primarily supports wiki quality control. The method wraps some
helpers to retrieve the wiki content for the given project(s), extract
URL(s) from the content, and return a list of link check results per
project wiki. Note that only the main wiki page is checked. As well,
this does not remove/replace the problematic link(s), and there still
may be false positive/negatives that may need to be reviewed manually.

## Usage

``` r
check_wiki_links(project_id, to_table = TRUE)
```

## Arguments

- project_id:

  Character vector of synapse project id(s) for which to get wiki.

- to_table:

  `TRUE` to return results as table or else keep as a list. Additional
  downstream operations may prefer one or the other.

## Value

Depending on `to_table`, a list or tibble of projects with links and
check results for links. The list will include projects without links
(as an empty list), while the table will omit projects without links.

## Examples

``` r
if (FALSE) { # \dontrun{
check_wiki_links(project_id = c("syn11374354","syn2343195"))
} # }
```
