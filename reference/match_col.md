# Find matching col in schema based on name

Synapse doesn't allow schemas to have columns of same name; this should
never return more than one.

## Usage

``` r
match_col(schema, col_name)
```
