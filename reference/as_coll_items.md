# Structure as collection items

Helper taking entity ids to create records used for dataset items *or*
dataset collection items. Collection items have the form
`list(entityId = id, versionNumber = x)`.

## Usage

``` r
as_coll_items(ids, item_version = c("abs", "stable"))
```

## Arguments

- ids:

  Ids of entities to make into dataset items.

- item_version:

  Integer for version that will be used for all items, e.g. 1.
  Otherwise, "latest" or "stable_latest". See details.

## Details

Note: For item version, dataset items allow two meanings of literal or
absolute "latest" vs. "stable_latest", but with files either one can be
used to mean the same thing since there will be correct interpretation
done under the hood. See implementation in `latest_version`.
