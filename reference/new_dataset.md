# Create new dataset with given items

Create new dataset with given items

## Usage

``` r
new_dataset(
  name,
  parent,
  items = NULL,
  item_version = NULL,
  folders = NULL,
  add_annotation_columns = TRUE,
  dry_run = TRUE
)
```

## Arguments

- name:

  Name of the dataset. It should be unique within the `parent` project.

- parent:

  Synapse id of parent project where the dataset will live.

- items:

  A list of id(s) of items to include for adding items *a la carte* to
  dataset. Usually the same parent project storing the files, but in
  some cases it may be a different project.

- item_version:

  Integer for version that will be used for all items, e.g. 1.
  Otherwise, "latest" or "stable_latest". See details.

- folders:

  A list of folder ids for adding all items in some folder(s). Can be
  used with `items`.

- add_annotation_columns:

  Whether to add annotation columns. Defaults to `TRUE`, same as the
  Python API.

- dry_run:

  If TRUE, don't actually store dataset, just return the data object for
  inspection or further modification.
