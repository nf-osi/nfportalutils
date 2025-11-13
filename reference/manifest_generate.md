# Generate manifest via schematic service

See [schematic manifest
generation](https://schematic.api.sagebionetworks.org/v1/ui/#/Manifest%20Operations/schematic_api.api.routes.get_manifest_route).
Note that this uses the access token of user that should already be
logged in with `syn_login`.

## Usage

``` r
manifest_generate(
  data_type,
  dataset_id = NULL,
  title = data_type,
  schema_url =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
  asset_view = "syn16858331",
  output_format = "google_sheet",
  use_annotations = TRUE,
  service = "https://schematic.api.sagebionetworks.org/v1/manifest/generate"
)
```

## Arguments

- data_type:

  Data type of the manifest to generate (aka Component).

- dataset_id:

  Optional, if given this fills out manifest for existing dataset
  instead of generating a blank manifest.

- title:

  Optional, custom title.

- schema_url:

  Optional, defaults to main NF 'latest' data model.

- asset_view:

  Optional, defaults to main NF portal fileview.

- output_format:

  Format of 'excel', 'google_sheet', or 'dataframe'. Defaults to
  'excel'.

- use_annotations:

  Use annotations if filling out manifest for existing dataset. Defaults
  to TRUE for NF.

- service:

  Service endpoint to use. Defaults to the schematic production
  endpoint.

## Value

For excel, path to local file; for google_sheet, URL to sheet; for
dataframe, JSON string of data.
