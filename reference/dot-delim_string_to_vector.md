# Convert a delimited string to vector, utility function.

Converts a delimited string to a stringlist annotation and adjust the
associated schema in the portal fileview.

## Usage

``` r
.delim_string_to_vector(string, sep, trim_ws = T)
```

## Arguments

- string:

  A character string.

- sep:

  Default = ",". The delimiter in the character string.

- trim_ws:

  Default = TRUE. Remove white space at the beginning and end of list
  items (e.g. "NF1, NF2" and "NF1,NF2" will yield the same STRING_LIST
  result).
