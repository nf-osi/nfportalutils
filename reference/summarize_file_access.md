# Summarize file access for files within some view

Some portion adapted from `@allaway`'s original script. Common usages
are:

- Check numbers of files are viewable and downloadable for registered
  Synapse users:
  `summarize_file_access(principal_id = 273948, access_type = "DOWNLOAD", "syn16858331")`

- Check that files within the portal purview are actually editable for
  the NF-OSI Sage Team, because we need at least edit permissions for
  updating annotations (historically, some issues with hackathon
  projects, etc.):
  `summarize_file_access(principal_id = 3378999, access_type = "UPDATE", "syn16858331")`

## Usage

``` r
summarize_file_access(principal_id, access_type, fileview_id)
```

## Arguments

- principal_id:

  Group(s) for which to check the access type.

- access_type:

  Which access type(s) to check for; result summarizes whether there are
  permissions for *all* types specified.

- fileview_id:

  Syn id of the view. View must include `benefactorId` and `type`.

## Details

More complex usage:

- Check access for *multiple* teams.

This summarizes file access by using the smaller set of benefactors, and
returns a `data.table` with columns `benefactorId`, `principalId`,
`access`, and `N`, which describes whether `principalId` has *all* the
specified `access` to `benefactorId` and its `N` files. This data can be
used for further aggregation as needed.
