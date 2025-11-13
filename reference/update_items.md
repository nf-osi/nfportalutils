# INTERNAL - apply updates to a collection of items

An internal transaction helper for trying to apply a changeset to a
collection, used in several higher-level collection utils. Given the
changeset that can represent updates of both types "replace" or "add",
this applies an update join keyed on `entityId` for the replace and
appends the new items to get the updated collection.

## Usage

``` r
update_items(current, update)
```

## Arguments

- current:

  List of lists representing a collection of items.

- update:

  Collection of items to apply as updates to `current_items`.
