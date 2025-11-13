# Simple bipartite representation in mermaid charts

Simple bipartite representation in mermaid charts

## Usage

``` r
bipartite_mmd_template(
  nodeset1,
  nodeset2,
  nodeset1_title = "INPUT",
  nodeset2_title = "OUTPUT",
  links = ""
)
```

## Arguments

- nodeset1:

  Character vector of one or more node ids. If named, nodes will use
  names instead of ids as labels.

- nodeset2:

  Character vector of one or more node ids. If named, nodes will use
  names instead of ids as labels.

- nodeset1_title:

  Title for nodeset1.

- nodeset2_title:

  Title for nodeset2.

- links:

  Optional, character vector of edges between nodes.
