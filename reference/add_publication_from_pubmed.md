# Add a publication to the publication table

Requires that the publication be in PubMed to auto-derive metadata such
as authors, title, etc. In contrast, `disease_focus` and `manifestation`
need to be supplemented by the curator. The `study_id` is used to get
consistent `studyName` and `fundingAgency` from study table without
manual input.

## Usage

``` r
add_publication_from_pubmed(
  pmid,
  study_id,
  disease_focus,
  manifestation,
  publication_table_id,
  study_table_id,
  dry_run = T
)
```

## Arguments

- pmid:

  PubMed ID (*not* PMCID) of the publication to be added.

- study_id:

  Synapse id(s) of the study that are associated with the publication.

- disease_focus:

  The disease focus(s) that are associated with the publication.

- manifestation:

  The manifestation(s) that are associated with the publication.

- publication_table_id:

  Synapse id of the portal publication table. Must have write access.

- study_table_id:

  Synapse id of the portal study table. Need read access.

- dry_run:

  Default = TRUE. Skips upload to table and instead prints formatted
  publication metadata.

## Value

If dry_run == T, returns publication metadata to be added.

## Examples

``` r
if (FALSE) { # \dontrun{
add_publication_from_pubmed(
               pmid = "33574490",
               study_id = "syn2343195",
               disease_focus = c("Neurofibromatosis"),
               manifestation = c("Meningioma"),
               publication_table_id = "syn16857542",
               study_table_id = "syn16787123")
} # }
```
