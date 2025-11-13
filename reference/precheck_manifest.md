# Precheck a manifest

Precheck before sending manifest off to schematic validation service.
Provides additional context and helpful recommendations.

## Usage

``` r
precheck_manifest(
  manifest_csv,
  official_props =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/modules/props.yaml"
)
```

## Arguments

- manifest_csv:

  Path to manifest_csv.

- official_props:

  (Optional) Doc listing official model attributes. Currently this
  requires the LinkML format.
