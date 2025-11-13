# Copy annotations

Copy annotations (all or selectively) from a source entity to one or
more target entities. If same annotation keys already exist on target
entities, the copy will replace the current values.

## Usage

``` r
copy_annotations(
  entity_from,
  entity_to,
  select = NULL,
  update = FALSE,
  as_list = TRUE
)
```

## Arguments

- entity_from:

  Syn id from which to copy.

- entity_to:

  One or more syn ids to copy annotations to.

- select:

  Vector of properties to selectively copy if present on the entity. If
  not specified, will copy over everything, which may not be desirable.

- update:

  Whether to immediately update or return annotation objects only.

- as_list:

  Only used when `update=FALSE`; for backwards-compatibility or when
  downstream usage of `copy_annotations` expects an R list, return as an
  R list.
