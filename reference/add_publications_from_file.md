# Add a batch of publications from spreadsheet

Add a batch of publications from spreadsheet

## Usage

``` r
add_publications_from_file(
  file,
  publication_table_id,
  study_table_id,
  list_sep = "|",
  dry_run = TRUE
)
```

## Arguments

- file:

  Spreadsheet (.csv/.tsv) with pubs to add should have `pmid`,
  `studyId`, `diseaseFocus`, `manifestation`. `pmid` is one per row and
  unique, rest can be `list_sep` vals.

- publication_table_id:

  Synapse id of the portal publication table. Must have write access.

- study_table_id:

  Synapse id of the portal study table. Need read access.

- list_sep:

  Delimiter character used to separate list columns.

- dry_run:

  Default = TRUE. Skips upload to table and instead prints formatted
  publication metadata.
