# Check fastq read pair matches samplesheet read pair assignment.

Read pairs are often encoded in the name of the file. Here, we check
that if encoded in the name of the file, that the samplesheet read pair
(e.g. \_1 or \_2) matches

## Usage

``` r
check_readpair_validity(
  samplesheet,
  parse_fun = function(x) gsub("_T[0-9]$", "", x)
)
```

## Arguments

- samplesheet:

  A local file or syn id of samplesheet.

- parse_fun:

  Function implementing how to parse samples in samplesheet.

## Examples

``` r
if (FALSE) { # \dontrun{
 check_readpair_validity('syn39542932')
 check_readpair_validity('syn29530880')
} # }
```
