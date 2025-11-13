# Generic template for genomic-type data file

Adapted from
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/make_meta.R#L65
Internal workhorse union of *all* the properties used for a genomic-type
data file – the sensible defaults/specific combination should be passed
in by a higher-level fun, e.g. `make_meta_maf`.

## Usage

``` r
make_meta_genomic_generic(
  cancer_study_identifier,
  genetic_alteration_type,
  datatype,
  stable_id = NULL,
  reference_genome_id = NULL,
  profile_name = NULL,
  profile_description = NULL,
  data_filename
)
```

## Arguments

- cancer_study_identifier:

  The study identifier.

- genetic_alteration_type:

  The cBioPortal generic alteration type.

- datatype:

  The cBioPortal data type of `data_filename`.

- stable_id:

  Stable id.

- reference_genome_id:

  Reference genome id, e.g. 'hg19'.

- profile_name:

  Name of the genomic profiling. This is set by the more specific
  `make_meta` utility. For example, "Mutations" for `make_*_maf` and
  "Copy-number alterations" for `make_*_cna`.

- profile_description:

  Brief description for the genomic profiling. This is set by the more
  specific `make_meta` utility.

- data_filename:

  Name of the data file that this meta file describes.
