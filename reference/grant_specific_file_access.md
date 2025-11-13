# Provide access to a specific set of files using a query result.

Sets READ/DOWNLOAD permissions for a specific user or team, provided a
vector of entity IDs. Generally, we do not set permissions this way, as
it can create many, many ACLs/"local sharing settings" which will need
to be removed at the time of data publication. However, at the time of
writing, one project (JHU Biobank) shares embargoed data but is required
to share only specific subsets of the files as needed by the data
requestor (e.g. only MPNST tumor data, or only RNA-seq data).

## Usage

``` r
grant_specific_file_access(
  principal_id,
  entity_ids,
  create_dataset = F,
  project_id = NULL,
  dataset_name = NULL
)
```

## Arguments

- principal_id:

  Synapse team or user id.

- entity_ids:

  Vector of entity ids.

- create_dataset:

  Optionally, create a dataset of the entity_ids, so that the user can
  easily retrieve them.

- project_id:

  If create_dataset=T, which project to create it in.

- dataset_name:

  Optional name for dataset to be created
