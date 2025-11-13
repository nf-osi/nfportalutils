# Transform table data to target schema for Synapse storage

**Currently implements list-schema features first and will do more
later.** Check and encode data values to expectations of Synapse target
table schema for storage. The target schema is more likely from an
existing table, since new tables can take advantage of `build_table`. To
get compatible list data, does JSON encoding and optionally
`list_truncate` when running into length limits. If truncation is not
OK, then the incompatibility will have to be resolved by updating schema
outside of this. Note that the setting applies to ALL list columns,
though it would be desirable to be column-specific.

## Usage

``` r
as_table_schema(df, schema, list_truncate = FALSE)
```

## Arguments

- df:

  A table, i.e. `data.frame`.

- schema:

  Table [schema
  object](https://python-docs.synapse.org/build/html/Entity.html#synapseclient.table.Schema)
  or Synapse id of target table from which to get schema.

- list_truncate:

  If length exceeds schema max for list columns, set `TRUE` to allow
  data truncation, `FALSE` to error only (default).

## Value

Synapse Table object ready for storing.
