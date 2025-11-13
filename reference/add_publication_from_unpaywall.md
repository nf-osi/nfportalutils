# Add a publication or preprint to the publication table via the Unpaywall API.

Add a publication to the publication table. Publication must be in
unpaywall database to retrieve info. For parameter-provided metadata
(e.g. "studyName"), function must a JSON-formatted character vector if
the destination Synapse column is of "STRING_LIST" format. Currently,
this function does not evaluate the schema, so this must be checked
manually.

## Usage

``` r
add_publication_from_unpaywall(
  publication_table_id,
  email_address,
  doi,
  is_preprint = F,
  preprint_server = NULL,
  study_name,
  study_id,
  funding_agency,
  disease_focus,
  manifestation,
  dry_run = T
)
```

## Arguments

- publication_table_id:

  The synapse id of the portal publication table. Must have write
  access.

- email_address:

  A valid email address. Is used to request metadata from the Unpaywall
  API. Please change the example to a real email address to help
  Unpaywall accurately track usage.

- doi:

  The DOI of the preprint to be added.

- is_preprint:

  Default = FALSE. Set to TRUE if DOI is from a preprint.

- preprint_server:

  Provide preprint server name. Must be one of 'bioRxiv', 'medRxiv',
  'chemRxiv', 'arXiv'

- study_name:

  The name(s) of the study that are associated with the publication.

- study_id:

  The synapse id(s) of the study that are associated with the
  publication.

- funding_agency:

  The funding agency(s) that are associated with the publication.

- disease_focus:

  The disease focus(s) that are associated with the publication.

- manifestation:

  The manifestation(s) that are associated with the publication.

- dry_run:

  Default = TRUE. Skips upload to table and instead prints formatted
  publication metadata.

## Value

If dry_run == T, returns publication metadata to be added.

## Examples

``` r
if (FALSE) { # \dontrun{
add_publication_from_unpaywall(publication_table_id = 'syn16857542',
               email_address = 'foo@bar.com',
               doi = '10.1074/jbc.RA120.014960',
               study_name = c(toJSON("Synodos NF2")),
               study_id = c(toJSON("syn2343195")),
               funding_agency = c(toJSON("CTF")),
               disease_focus = "Neurofibromatosis 2",
               manifestation = c(toJSON("Meningioma")),
               dry_run = T)
} # }
```
