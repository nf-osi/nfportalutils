# Wrapper to create data-driven flowchart with pretty processing provenance mermaid template

This generates a mermaid file, which can be rendered live/further edited
in

## Usage

``` r
processing_flowchart(report)
```

## Arguments

- report:

  Which report to determine subset of data for which to generate
  flowchart fig.

## Examples

``` r
if (FALSE) { # \dontrun{
flowchart <- processing_flowchart(report = "2023-MY")
cat(flowchart, file = "flowchart.mmd")
} # }
```
