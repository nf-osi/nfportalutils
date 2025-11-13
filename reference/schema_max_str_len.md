# Consult schema about max string length

Utility to query the schema regarding the max string length for a key
based on current valid values. If the key values are not constrained
(free-text), a default string length of 100 is returned. If not in
schema, returns `NA`. TO DO: related fun to consult schema about type
(integer, string, stringlist, etc.) TO DO: warn if key is not actually
string or stringlist type

## Usage

``` r
schema_max_str_len(
  key,
  schema =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
  parent_context = "bts",
  default = 100
)
```

## Arguments

- key:

  Schema key (not label).

- schema:

  URL or local path to a .jsonld file which the schema is to be read
  from.

- parent_context:

  Default = bts. The JSON-LD context for the value in question.

- default:

  Default string length to use for keys without constrained values.
