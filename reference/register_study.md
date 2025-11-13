# Register a NEW project for the NF Data Portal in **Portal - Project View**

Add relevant study metadata to the project as annotations. Add to scope
of NF-OSI data portal and management in **Portal - Project View**.

## Usage

``` r
register_study(
  id,
  study_meta,
  summary,
  study_summary_table,
  portal_project_view = "syn52677631"
)
```

## Arguments

- id:

  Synapse id of study.

- study_meta:

  List of annotations representing study meta.

- summary:

  Large summary string.

- study_summary_table:

  Id of where to store summary (can be any table with a `summary`
  LARGETEXT column).

- portal_project_view:

  View of DCC-managed projects (studies).
