# Create a strictly new project

Internal handler for creating a project that first checks whether
project already exists and disallows overwriting. For a less strict
version that allows overwriting with a warning, e.g. named
`update_project`, implement with `createOrUpdate = TRUE` and then
compare createdOn and modifiedOn to issue a warning (which would be more
informative than current Python client).

## Usage

``` r
new_project_strict(project_name)
```

## Arguments

- project_name:

  Name of project to be created.
