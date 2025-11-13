# Exporting Data to Other Platforms: cBioPortal

**Document Status:** Working  
**Estimated Reading Time:** 8 min

## Special acknowledgments

This workflow heavily adapted some code originally written by
[hhunterzinck](https://github.com/hhunterzinck), a former Senior Data
Scientist at Sage.

## Important notes

The requirements for cBioPortal change, just like with any other
software or database system. The workflow also corresponds to the NF-OSI
processing SOP, for known processed data outputs, so reuse more
carefully for anything outside this SOP. Thus, while we try to maintain,
test, and add features to this workflow to accommodate new scenarios on
a yearly submission basis, there may be occasional points when things
are broken with regard to external dependencies or misaligned with the
SOP.

## Intro

This describes how to package some Synapse processed data as a
cBioPortal study dataset. A cBioPortal study can contain one or more
data types (see [cBioPortal
docs](https://docs.cbioportal.org/file-formats/)), but at minimum must
contain mutations data. The current API covers creating a cBioPortal
study with a subset of data types relevant to the NF workflow (so not
all data types). The design has been inspired by and should feel
somewhat like working with the R package
[usethis](https://github.com/r-lib/usethis), where data types can be
added to the study package interactively.

Though some data type-specific sanity checks are run when that data type
is added, **final [validation](#Validation) should be done with the
official cBioPortal validation tools/scripts**.

## Set up

First load the `nfportalutils` package and log in. The recommended
default usage of `syn_login` is to use it without directly passing in
credentials. Instead, have your token stored at the `SYNAPSE_AUTH_TOKEN`
environment variable.

``` r
library(nfportalutils)
syn_login()
```

## Create a new study dataset

Create the study dataset “package” where we can put together the data.
Again, each study dataset combines multiple data types – clinical, gene
expression, gene variants, etc. This will also set the working directory
to the new study directory.

``` r

cbp_new_study(cancer_study_identifier = "npst_nfosi_ntap_2022",
              name = "Plexiform Neurofibroma and Neurofibroma (Pratilas 2022)",
              type_of_cancer = "nfib", # required -- see https://oncotree.mskcc.org/
              citation = "TBD")
```

## Add data types to study

Data types can be added *in any order* using the `cbp_add*` functions,
which try to do all that is needed for a data type. A `cbp_add*`
function downloads data, may implement light reformatting, creates the
data file, create the meta files, may create other accessory files, and
may run data type-specific sanity checks.

If needed, the meta can be edited after the file has been created.
Defaults are for known NF-OSI processed data outputs. If these defaults
don’t apply because this was ad hoc processing or some variation in the
SOP, it is recommended to take a look at the lower-level utils
`make_meta_*` or understand how to edit the files manually.

(Reminder) These should be run with the working directory set to the
study directory as set up above to ensure consistent metadata.

### Add mutations data

- `maf_data` references a final merged maf output file from the NF-OSI
  processing pipeline (vcf2maf) OK for public release.
- Under the hood, a required case list accessory file is also generated.

``` r

maf_data <- "syn36553188"

cbp_add_maf(maf_data)
```

### Add copy number alterations (CNA) data

- `cna_data` is expected to be a `.seg` file on Synapse.

``` r

cna_data <- "syn********"

cbp_add_cna(cna_data)
```

### Add expression data

- `expression_data` is expected to be a `.txt` called `gene_tpm.tsv`
  file on Synapse.
- The NF-OSI can include the raw expression data as well, called
  `gene_counts.tsv`, but this can usually be omitted.
- These NF-OSI outputs will be somewhat modified in translation to have
  the required headers.

``` r

mrna_data <- "syn********"
mrna_data_raw <- "syn********"

cbp_add_expression(mrna_data,
                   expression_data_raw = mrna_data_raw) # optional
```

### Add clinical data

Clinical data can include both Patient and Sample clinical data, where
Sample file is required, whereas the Patient file is optional
(<https://docs.cbioportal.org/file-formats/#clinical-data>). However,
some attributes are considered Patient-only so a Patient file must be
created.

Thus, `clinical_data` is typically prepared from an existing Synapse
table. Most NF clinical data – `individualId`, `specimenID`, `age`,
`sex`, `tumorType`, etc. – are in one table because of the current
annotation approach, so we treat this like a Sample table. The workflow
will handle creating a Patient file if some Patient-specific attributes
are detected. However, in cases where patient:sample is not 1:1
(multiple samples per patient) or the project already has both Patient
and Sample tables set up by the contributor, then using two tables is
natural and relatively straightforward. One need not worry about using
materialized view for 1:1 data to get to a single table. The examples
below will show how to work with both options.

Important to clinical data addition is a global export list (mapping) of
clinical variables that clinical data export relies on. In the below
examples, this list is referred to as `ref_map`. This resource serves
several purposes:

- Provides a mapping that keeps up-to-date with what cBioPortal,
  e.g. what is patient-only and what is the closest standard variable an
  NF variable should map to. Sincere effort is given to make data
  comparable across different public datasets.
- Serves as export control; only variables in the list will be exported.
  Clinical data requires thoughtful selection and documentation of what
  clinical variables to be made public. The original clinical data on
  Synapse may possibly contain variables more sensitive than appropriate
  for cBioPortal, or the original clinical data may contain variables
  not super relevant for visualization on cBioPortal
  (e.g. `link_to_WGS_file`).

While `ref_map` controls the selection of clinical variables, the
selection of patients/samples appropriate for the current dataset is
done via the query used during this process. That is, the clinical
table(s) must sometimes be subsetted to match what is actually released
in the current study dataset. For example, if the full clinical cohort
table comprises patients 1-50, but the dataset can only release data for
patients 1-20 for expression data and data patients 15-20 for CNA data,
then use a query that makes selection for patients 1-20 only. In other
cases, the selection query may look more like `where batch = 'batch1'`.

Remember to specify `ref_map`. It may be helpful to follow the link to
better understand the file.

    ref_map <- "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/mappings/cBioPortal/cBioPortal.yaml"

##### New clinical variables

If the current dataset to be exported contains new clinical data
variables, the variables should be reviewed and added to
[`ref_map`](https://github.com/nf-osi/nf-metadata-dictionary/blob/main/mappings/cBioPortal/cBioPortal.yaml)
as appropriate.

##### NA value recoding

By default, the package converts a comprehensive set of missing/unknown
values to cBioPortal’s preferred empty string format. Users can
customize which values are treated as NA by setting the package option:

``` r
# View current NA recode values
getOption("nfportalutils.na_recode")

# Customize NA values (example)
options(nfportalutils.na_recode = c("NA", "NULL", "missing", "custom_value"))
```

#### (Option 1) Export using single source clinical data table

``` r

 # example query where a subset of patients/samples are releasable
clinical_data <- "select * from syn43278088 where batch = 'batch1'"

cbp_add_clinical(clinical_data, ref_map)
```

#### (Option 2) Export using both Patient and Sample clinical data table

``` r

# example queries when all patients/samples are releasable
cd_sample <- "select * from syn5556216"
cd_patient <- "select * from syn7342635"

cbp_add_clinical(cd_sample, ref_map, type = "sample")
cbp_add_clinical(cd_patient, ref_map, type = "patient")
```

## Validation

Validation has to be done with a cBioPortal instance (use their official
Docker image).

For an example simple validation, assuming your current wording
directory is something like `~/sage/submissions` with a study folder
called `npst_nfosi_ntap_2022` run validation like:

    STUDY=npst_nfosi_ntap_2022
    sudo docker run --rm --user $(id -u):$(id -g) -v $(pwd):/studies cbioportal/cbioportal:6.3.2  validateStudies.py -d /studies -n -html /studies -l $STUDY

A log .txt file as well as prettier .html file for human consumption
will be outputted containing validation results. Submission can proceed
as along as there are no errors.

**See the [general docs for dataset
validation](https://docs.cbioportal.org/using-the-dataset-validator) for
more examples.**
