# Calculate byte budget for a schema

Tables have a hard width limit of 64KB. Given a current table schema,
this does math for how many bytes remain or are already allocated.
Useful as an austerity measure if indeed one has a very large table, or
in other cases when philosophically being more principled in schema
configuration.

## Usage

``` r
byte_budget(table, schema_cols = NULL, result = "remaining")
```

## Arguments

- table:

  Existing Synapse table id or the table schema object used to retrieve
  the column types.

- schema_cols:

  Optional, this also can take a list of column characteristics; use
  when building from scratch and columns are not yet stored. If given,
  `table` will be ignored.

- result:

  Return the summary number for "remaining" or "allocated", or return a
  TRUE/FALSE for "within" budget.

## Details

See also:

- https://rest-docs.synapse.org/rest/org/sagebionetworks/repo/model/table/ColumnType.html
