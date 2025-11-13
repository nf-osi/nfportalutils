# Add to collection

Add items(s) to an *existing* collection, using the item(s)' current
(latest) version. For datasets, the items should be files. For dataset
collections, the items should be datasets. If an item attempting to be
added happens to already be in the collection, this might lead to
version conflicts, so the update will be rejected unless `force` is
true.

## Usage

``` r
add_to_collection(collection_id, items, check_items = FALSE, force = FALSE)
```

## Arguments

- collection_id:

  Collection id.

- items:

  Character vector of one or more dataset entity ids to add.

- check_items:

  Whether to check that ids are really appropriate item types and remove
  non-appropriate item types to help avoid Synapse errors (default
  `FALSE` because in most cases `items` are curated, and using check
  will be slower).

- force:

  If some items are currently in the collection with a different
  version, should these items be force-added using current version? The
  safe default is `FALSE` to ensure any such updates are intentional.

## Details

This is implemented with lower-level REST API because the Python client
(as of v2.7) doesn't yet implement dataset collection class and methods
(but dataset and relevant methods like `add_item` method are available).
Thus, while this is generic enough to handle both datasets and dataset
collections it is expected to be used more for dataset collections given
that the dataset method is provided.
