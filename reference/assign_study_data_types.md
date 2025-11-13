# Summarize data types for the study

Data types are summarized, or "rolled-up", for the study based on its
child file annotations. Summary values are added back as and overwrites
the current `dataType` annotation for the study. See also the related
`update_study_annotations`, where study-level annotations are *rolled
down* to child files. Note that under-the-hood this now wraps a
generalized util `summarize_attribute`.

## Usage

``` r
assign_study_data_types(
  study_table_id,
  fileview_id,
  id_col = "studyId",
  attribute = "dataType",
  dry_run = TRUE
)
```

## Arguments

- study_table_id:

  Synapse ID of reference portal study table. Used to get study ids.

- fileview_id:

  Synapse ID of the reference portal fileview.

- id_col:

  Name of the study id column in `study_table_id` and `fileview_id`.
  Defaults to `studyId`.

- attribute:

  Attribute being summarized using fileview. Defaults to `dataType`.

- dry_run:

  Default = TRUE. Whether to update as well or just return list of
  annotation objects.

## Value

List of annotations objects.

## Examples

``` r
if (FALSE) { # \dontrun{
assign_study_data_types(study_table_id = 'syn52694652',
                        fileview_id = 'syn16858331',
                        id_col = 'studyId',
                        attribute = 'dataType',
                        dry_run = T)
} # }
```
