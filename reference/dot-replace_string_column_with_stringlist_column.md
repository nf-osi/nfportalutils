# Replace string column with stringlist column

Guts of this ripped from @jaeddy gist
(https://gist.github.com/jaeddy/1cf49f7851945beedb39d431134734af)

## Usage

``` r
.replace_string_column_with_stringlist_column(
  table_id,
  column_name,
  max_str_len
)
```

## Arguments

- table_id:

  A synapse entity id.

- column_name:

  The column name of relevant column to modify.

- max_str_len:

  Max string length to be set in schema of new column.
