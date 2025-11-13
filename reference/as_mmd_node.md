# Generate notation for mermaid.js nodes

Generate notation for mermaid.js nodes

## Usage

``` r
as_mmd_node(entity, class = c("Project", "Dataset", "Folder"))
```

## Arguments

- entity:

  Character vector of one or more entity ids. If named, nodes will use
  names instead of ids as labels. Note that entity ids starting with
  "\_" are considered blank nodes and are treated specially.

- class:

  Optional, add a class to the node.
