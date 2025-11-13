# Helper summarization util

Given some table X that provides values, summarize the values for an
attribute and add summary as annotations on some entity. The entity
could be X itself or another entity Y, e.g. a parent container entity.
Example 1: With datasets, summarize `species` for all the files. Example
2: With projects, summarize `dataType` for all the files (in fact, see
`assign_study_data_types`).

## Usage

``` r
summarize_attribute(
  summary_query,
  attribute,
  entity_id = NULL,
  dry_run = TRUE,
  check_fun = NULL
)
```

## Arguments

- summary_query:

  Query (usually of a fileview) that returns appropriate aggregation per
  row. You may need to add `group_concat`, `distinct`, and or `unnest`
  to the query to get the correct list of distinct values depending on
  your data
  (e.g.`select group_concat(distinct unnest(tumorType)) as tumorType from ...`).

- attribute:

  Name of attribute to update as annotation.

- entity_id:

  Either a single valid Synapse id of the entity for which to update the
  attribute *or* a column present in `summary_query` that stores ids.

- dry_run:

  Default = `TRUE`. Whether to update as well or just return list of
  annotation objects.

- check_fun:

  An optional custom check function to apply to the values being updated
  in order for update to go through. Should return a boolean. Used only
  if dry_run = `FALSE`. It can be tailored towards the attribute/entity
  being updated (i.e. taking into account the schema and valid values).
