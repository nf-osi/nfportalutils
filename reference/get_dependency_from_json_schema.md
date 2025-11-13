# Get dependencies for node in JSON-LD schema

Shorthand for getting props defined in annotation template using
`get_by_prop_from_json_schema` under the hood.

## Usage

``` r
get_dependency_from_json_schema(
  id,
  prop = "sms:requiresDependency",
  schema =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
  return_labels = TRUE,
  recursive = TRUE,
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
