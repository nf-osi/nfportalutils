# Derive annotations for processed output data

A processed or derived file can inherit annotations from the input
file(s). Currently, this generously facilitates inheritance of many
properties except ones that "obviously" shouldn't be inherited, such as
"fileFormat" or "comments". These rules are hard-coded and might need to
be expanded as the data model changes.

## Usage

``` r
derive_annotations(
  sample_io,
  template = NULL,
  schema =
    "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
  use_sample_as_specimen_id = FALSE,
  verbose = TRUE
)
```

## Arguments

- sample_io:

  Mapping of input and output files from a workflow.

- template:

  Which template to use when deriving annotations. This controls which
  attributes are relevant for transfer/keep. If not given, will use
  whatever is set for the attribute "template".

- schema:

  Reference to data model source.

- use_sample_as_specimen_id:

  If TRUE, sets specimenID from the sample column (parsed from directory
  structure/filenames) instead of inheriting from input files. Useful
  when directory structure provides more accurate specimen identifiers.

- verbose:

  Whether to output detailed messages.

## Details

If multiple inputs given, this will inherit annotations from the FIRST
input.
