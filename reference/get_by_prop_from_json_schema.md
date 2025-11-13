# Look up connected nodes by specified property in JSON-LD schema

Use with schematic-generated JSON-LD schema: given `@id`, get connected
nodes by specified prop (e.g. `sms:something`). Intended to be a generic
used to define more specific lookup utils. Can do recursive lookup,
though graph should be a tree/acyclic (!). (Useful for props such as
`dependsOn`, doesn't make sense for props such as `rdfs:label`.)

## Usage

``` r
get_by_prop_from_json_schema(
  id,
  prop,
  schema =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
  return_labels = TRUE,
  recursive = FALSE,
  result = NULL,
  rest = NULL
)
```

## Arguments

- id:

  Id (`@id`) for which to get range values; include prefix if needed.

- prop:

  Property; include prefix if needed.

- schema:

  Path (URL or local) to file from which schema will be read, or schema
  as list object.

- return_labels:

  Return labels (default), otherwise ids of connected nodes.

- recursive:

  Recursive lookup?

- result:

  Vector of accumulated results; used for recursive lookup.

- rest:

  Vector of remaining ids; used for recursive lookup.
