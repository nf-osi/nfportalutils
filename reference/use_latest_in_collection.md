# Update item versions to "latest" in a collection

Update an *existing* collection so that all items or a subset of items
reference their latest version. Should work for both datasets
(collection of files) and dataset collections (collection of datasets).

## Usage

``` r
use_latest_in_collection(
  collection_id,
  items = "all",
  version_semantics = "abs"
)
```

## Arguments

- collection_id:

  Collection id.

- items:

  Vector of dataset ids for which to update reference to latest version,
  or "all" (default) to update all.

- version_semantics:

  Use "abs" for absolute latest version or "stable". Only used for
  collection entities. See details.

## Details

Semantics of "latest" – "abs" vs "stable": Datasets and dataset
collections always start out as draft, so unlike other entities there is
a concept of a "stable" version which is the *real* latest, but which
might not always exist (i.e. there is only draft version because no
stable version has ever been created). For datasets/dataset collections
the latest version refers to a DRAFT, so latest stable version is
`versionNumber` - 1 under the condition that the `versionNumber` is
greater or equal to 2. When `versionNumber` = 1 and `isLatestVersion` is
TRUE, this means there is not yet a stable version. When using stable
version semantics, if a stable version does not exist an error will be
thrown.
