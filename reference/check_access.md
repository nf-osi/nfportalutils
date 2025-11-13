# Check access

Check access

## Usage

``` r
check_access(
  id,
  principal_id,
  access_type = c("CREATE", "UPDATE", "CHANGE_SETTINGS", "DOWNLOAD", "MODERATE", "READ",
    "CHANGE_PERMISSIONS", "DELETE")
)
```

## Arguments

- id:

  The benefactor entity.

- principal_id:

  Group(s) for which to check the access type.

- access_type:

  Which access type(s) to check for; result summarizes whether there are
  permissions for *all* types specified.
