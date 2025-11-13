# Update the People table from a source Table or View column

Update the People table from a source Table or View column

## Usage

``` r
add_people_from_table(
  people_table_id,
  people_column,
  source_table_id,
  source_column,
  dry_run = T
)
```

## Arguments

- people_table_id:

  The synapse id of the table used for referencing people.

- people_column:

  Column name within the people table that contains the relevant people
  values.

- source_table_id:

  The synapse id of the source table.

- source_column:

  Column name within the source table that contains the relevant source
  values.

- dry_run:

  Default = TRUE Skips upload of annotations unless set to FALSE.

## Value

If dry_run == T, prints preview of updated people table, otherwise
uploads the updates.
