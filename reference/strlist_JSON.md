# Convert delimited record to JSON representation needed by a stringlist col schema

Internal helper that reuses and extends the utility of
`.delim_string_to_vector`.

## Usage

``` r
strlist_JSON(record, sep = ",", trim_ws = T)
```

## Arguments

- record:

  Character vector of length one representing a single record.

- sep:

  Default = ",". The delimiter in the character string.

- trim_ws:

  Default = TRUE. Remove white space at the beginning and end of list
  items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST
  result).
