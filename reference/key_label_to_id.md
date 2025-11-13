# Query for schema key id given label

Utility to translate label to id using a schematic-generated schema.

## Usage

``` r
key_label_to_id(
  label,
  prefixed = TRUE,
  schema =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld"
)
```

## Arguments

- label:

  The term label, a.k.a display name.

- prefixed:

  Boolean to indicate whether to include namespace prefix or return bare
  ID. Defaults to `TRUE`.

- schema:

  URL or local path to a .jsonld file which the schema is to be read
  from.

## Value

The id if found, such as "bts:MyID", otherwise an empty character
vector.
