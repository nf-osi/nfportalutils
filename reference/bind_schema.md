# Wrapper for JSON schema binding

Binds a JSON schema to a Synapse entity. If a schema is already bound to
the entity, running this function again will replace the existing schema
binding with the new one.

## Usage

``` r
bind_schema(id, schema_id, derived_annotations = FALSE)
```

## Arguments

- id:

  Id of entity to which schema will be bound

- schema_id:

  Schema id as registered on Synapse. Examples for "latest" or
  explicitly versioned schemas: "org.synapse.nf-portalstudy",
  "org.synapse.nf-rnaseqtemplate-10.2.0".

- derived_annotations:

  Whether to enabled derived annotations. Default `FALSE` as this is the
  API default.

## Details

See https://help.synapse.org/docs/JSON-Schemas.3107291536.html

## Examples

``` r
if (FALSE) { # \dontrun{
# Bind a dataset schema to a folder
bind_schema(id = "syn12345678", schema_id = "org.synapse.nf-rnaseqtemplate-10.2.0")

# Bind with derived annotations enabled
bind_schema(id = "syn12345678",
            schema_id = "org.synapse.nf-protocol",
            derived_annotations = TRUE)

# Replace an existing schema binding with a new one
bind_schema(id = "syn12345678", schema_id = "org.synapse.nf-rnaseqtemplate-11.0.0")
} # }
```
