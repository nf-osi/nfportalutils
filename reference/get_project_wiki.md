# Get wiki content of synapse project(s)

Get the wiki object or text content (main page only). This is primarily
a helper function used in QC but may be useful for other wiki analysis.

## Usage

``` r
get_project_wiki(project_id, markdown = TRUE)
```

## Arguments

- project_id:

  Character vector of synapse project id(s) for which to get wiki.

- markdown:

  If `TRUE` (default) return only the markdown text, else return full
  wiki object.

## Value

A list storing the wiki object or markdown-formatted text.

## Examples

``` r
if (FALSE) { # \dontrun{ 
txt <- get_project_wiki(c("syn11374354","syn2343195"))
} # }
```
