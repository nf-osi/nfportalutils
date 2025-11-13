# Read and use a mapping file

The mapping file should be a YAML or JSON format file that minimally has
a mapping key storing the translations of source data model element to
the target data model. See [example file for
NF](https://github.com/nf-osi/nf-metadata-dictionary/tree/main/mappings).
This util reads the mapping file, does some light checking, and creates
a list object that downstream functions can use.

## Usage

``` r
use_ref_map(ref_map, as_dt = TRUE)
```

## Arguments

- ref_map:

  YAML or JSON mapping. See details.

- as_dt:

  Return as `data.table`, the default, otherwise do checking but just
  return the list representation, which retains some metadata.

## Value

Either a list of lists storing `source`, `label`, `description`,
`data_type`, `attribute_type` or a `data.table` representation.

## Details

If the specification for the mapping file is later formalized with a
different structure this should be handled accordingly (that is to say,
this is one of the under-the-hood things that can change).
