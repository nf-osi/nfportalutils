# Adjust schema max size based on hint

Note: For STRING cols, the hard limit is 1000 char size, though at 250
using LARGETEXT is officially recommended, so possibly at that
breakpoint this should just create the different column type instead of
increasing size.

## Usage

``` r
adjust_string_size(view, hint, check_byte_budget = TRUE)
```
