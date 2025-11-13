# Updates a set of files with project-level annotations.

Adds studyId, studyName, initiative, funder annotations to a set of
files from a fileview query. Assumes that projectId==studyId. You must
have the Synapse default view column "type" present in your view. After
running this function, changes may take a few minutes to propagate
through Synapse.

## Usage

``` r
update_study_annotations(study_table_id, fileview_id, annotations, dry_run = T)
```

## Arguments

- study_table_id:

  The synapse id of the portal study table.

- fileview_id:

  The synapse id of a fileview. Must have the desired annotations in the
  schema, and must have the files to annotate included in the scope.
  Must have write access to the files you want to re-annotate.

- annotations:

  A vector of annotations to gather from the study table, and assign to
  the files.

- dry_run:

  Default = TRUE Skips upload of annotations unless set to FALSE.

## Value

If dry_run == T, returns updated annotations as a tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
update_study_annotations(study_table_id = "syn16787123",
                         fileview_id = "syn16858331",
                         annotations = c("studyId","studyName","initiative","fundingAgency"),
                         dry_run = T)
} # }
```
