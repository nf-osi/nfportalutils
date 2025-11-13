# List datasets in project

Return a list of datasets. Datasets can be folders in the expected
location in the project or actual dataset entities. Note that
dataset-folders will always be what exists first in the project, as sort
of the dataset precursor. The files in dataset-folders can be translated
to dataset entities later. When not found, will return NULL w/
explanatory message.

## Usage

``` r
list_project_datasets(project_id, type = c("folder", "dataset"))
```

## Arguments

- project_id:

  Synapse project id.

- type:

  Whether to list datasets as immediate folders under "Raw Data" root
  (default, see details) or actual dataset entities in the project.
