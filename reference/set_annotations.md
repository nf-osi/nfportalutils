# Wrapper around the Python `set_annotations` that pulls current annotations and adds new annotations with given annotations data or replaces data for annotations with the same keys existing on the entity.

Wrapper around the Python `set_annotations` that pulls current
annotations and adds new annotations with given annotations data or
replaces data for annotations with the same keys existing on the entity.

## Usage

``` r
set_annotations(id, annotations)
```

## Arguments

- id:

  Synapse entity id.

- annotations:

  A flat list representing annotation key-value pairs, e.g.
  `list(foo = "bar", rank = 1, authors = c("jack", "jane"))`
