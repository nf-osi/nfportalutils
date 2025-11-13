# Initialize a new cBioPortal study dataset

Create a new directory with a basic required [study meta
file](https://docs.cbioportal.org/file-formats/#meta-file), much like
how we'd create a new R package and put a DESCRIPTION file in it.

## Usage

``` r
cbp_new_study(
  cancer_study_identifier,
  name,
  type_of_cancer,
  description =
    "The data are contributed by researchers funded by the Neurofibromatosis Therapeutic Acceleration Program (NTAP). The reprocessing of the raw data is managed by the NF Open Science Initiative (https://nf.synapse.org/).",
  short_name = NULL,
  citation = NULL,
  pmid = NULL,
  groups = "PUBLIC",
  add_global_case_list = TRUE,
  validate = TRUE,
  verbose = TRUE
)
```

## Arguments

- cancer_study_identifier:

  Cancer study identifier in format such as `nst_nfosi_ntap_2022`.

- name:

  Name of the study, e.g. "Malignant Peripheral Nerve Sheath Tumor
  (NF-OSI, 2022)".

- type_of_cancer:

  Id for type of cancer. If `validate` is TRUE, this is one of the
  things validated with warning if mismatched.

- description:

  Description of the study, defaults to a generic description that can
  be edited later.

- short_name:

  (Optional) Short name for the study.

- citation:

  (Optional) A relevant citation, e.g. "TCGA, Nature 2012".

- pmid:

  (Optional) One or more relevant pubmed ids (comma-separated, no
  whitespace); if used, citation cannot be `NULL`.

- groups:

  (Optional) Defaults to "PUBLIC" for use with public cBioPortal;
  otherwise, use group names that makes sense with the configuration of
  your cBioPortal instance.

- add_global_case_list:

  (Optional) Use `NULL` to ignore, default is `TRUE` for an "All
  samples" case list( to be generated automatically.

- validate:

  Validate against public cBioPortal configuration. Default `TRUE`, but
  might want to set to `FALSE` especially if using a custom cBioPortal
  instance with different configuration.

- verbose:

  Whether to be chatty.
