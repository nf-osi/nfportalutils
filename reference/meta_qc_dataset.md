# QC dataset metadata with pass/fail result

R wrapper for validation workflow with schematic. Because there is no
validation-only service endpoint, we move metadata around twice
(generating manifest from server and submitting back to server), so once
schematic has a validation-only service endpoint that would be much more
efficient. A dataset in this context is a folder, usually tagged with
`contentType` = "dataset".

## Usage

``` r
meta_qc_dataset(
  dataset_id,
  data_type = NULL,
  asset_view = "syn16858331",
  schema_url =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
  cleanup = TRUE,
  depth = 1L
)
```

## Arguments

- dataset_id:

  Id of folder that represents a dataset, not actual Synapse dataset
  entity – see details.

- data_type:

  A specific data type to validate against, otherwise tries to infer
  based on annotations. See details.

- asset_view:

  A reference view, defaults to the main NF portal fileview.

- schema_url:

  Schema URL, points by default to 'latest' main NF schema, can change
  to use a specific released version.

- cleanup:

  Whether to automatically remove reconstituted manifests once done.
  Default `TRUE`.

- depth:

  How much deeper to go when there appears to be no files in the
  immediate scope. Defaults to 1L.

## Value

List of structure `list(result = result, notes = notes)`, where `result`
indicates passing or `NA` if no data or if couldn't be validated for
other reasons.

## Details

Note that we prefer to wrap the schematic web API over a local
installation because:

- Will not require user to go through local schematic setup for this to
  be functional

- API more likely reflects an up-to-date version of schematic and
  consistent with current DCA deployment

When `data_type` can't be inferred based on annotations, this is treated
as a fail.

Status: alpha and likely to change based on changes in `schematic`.
