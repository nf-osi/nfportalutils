# Validate manifest via schematic service

See [schematic
validation](https://schematic.api.sagebionetworks.org/v1/ui/#/Model%20Operations/schematic_api.api.routes.validate_manifest_route).
Get validation results from schematic service. Downstream utils can
consume these results for custom display/report.

## Usage

``` r
manifest_validate(
  data_type,
  json_str = NULL,
  file_name = NULL,
  restrict_rules = FALSE,
  schema_url =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
  service = "https://schematic.api.sagebionetworks.org/v1/model/validate"
)
```

## Arguments

- data_type:

  Data type of the manifest to generate (aka Component).

- json_str:

  JSON string representing metadata.

- file_name:

  Path to file, has to be `.csv`. Ignored if `json_str` is given.

- restrict_rules:

  Use only basic schematic validation instead of extended validation
  with Great Expectations, default `FALSE`.

- schema_url:

  Optional, defaults to main NF 'latest' data model.

- service:

  Service endpoint to use. Defaults to the schematic production
  endpoint.
