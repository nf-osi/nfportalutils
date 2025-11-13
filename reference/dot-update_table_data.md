# Replace/update table contents = input data must have ROW_ID and ROW_VERSION columns to update, otherwise will append data.

Replace/update table contents = input data must have ROW_ID and
ROW_VERSION columns to update, otherwise will append data.

## Usage

``` r
.update_table_data(table_id, new_data, etag = NULL)
```

## Arguments

- table_id:

  The synapse id of the table to update.

- new_data:

  The updated table.

- etag:

  An etag of the latest version of the table. If not provided, will
  query table_id to retrieve latest etag.
