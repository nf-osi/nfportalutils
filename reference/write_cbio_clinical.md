# Write cBioPortal clinical file

Wrapper function for creating clinical files. There are two: `PATIENT`
and `SAMPLE`. The `PATIENT` file is actually optional, so there are only
checks for making sure `SAMPLE` can be created. `df` is expected to be a
table containing clinical data available, and maybe even some irrelevant
data (since NF data is not well-normalized and there may be custom meta
from the contributor).

## Usage

``` r
write_cbio_clinical(
  df,
  ref_map,
  clinical_type = NULL,
  na_recode = getOption("nfportalutils.na_recode"),
  delim = "\t",
  publish_dir = ".",
  verbose = TRUE
)
```

## Arguments

- df:

  A `data.frame` representing clinical dataset to publicize.

- ref_map:

  YAML or JSON mapping. See details.

- clinical_type:

  (Optional) Export `df` *only* as either `SAMPLE` or `PATIENT` file?
  When not specified, one or both is inferred and created based on the
  data.

- na_recode:

  NA values should be replaced with a blank string (which seems to be
  standard) in exported file.

- delim:

  Delimiter character used for writing file, defaults to tab-delimited
  per cBioPortal specs.

- publish_dir:

  Directory path to write to, defaults to current.

- verbose:

  Report where file has been written.

## Details

This depends on a `ref_map` specification to know which clinical data to
include for cBioPortal and how to segregate the clinical attributes into
the right files. Basically, `ref_map` decides what variables can be made
public and how they should be represented in cBioPortal. For example,
given a table `T` on Synapse with variables A-Z and mappings in
`ref_map` for A-C + L-M, we take the intersection of variables present.
But first, check that *required* variables in *ref_map* are present. So
first the subset `df` is created from `T`.
