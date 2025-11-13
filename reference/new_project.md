# Create a new project

Set up a new NF project with wiki, folders, fileview, and permissions.
Most parameters come from a project intake & data sharing plan (DSP)
form. Aside from default folders, folders are tailored for data
mentioned in DSP. The NF-OSI team is hard-coded to be admin in addition
to the funder team indicated by `funder`. Since this is intended for
actual new projects, it will fail if a same existing project is
detected.

## Usage

``` r
new_project(
  name,
  pi,
  lead,
  admin_user = NULL,
  abstract,
  institution,
  funder,
  initiative,
  datasets = NULL,
  other_resources = NULL,
  publicview = FALSE,
  webview = FALSE,
  ...
)
```

## Arguments

- name:

  Name of the project/study.

- pi:

  Name of the principal investigator.

- lead:

  Name(s) of the project lead/data coordinator, comma-sep if multiple,
  e.g. "Jane Doe, John Doe".

- admin_user:

  (Optional) Single id or list of ids of users to be made admin(s).

- abstract:

  Project abstract/description.

- institution:

  Affiliated institution(s), **semicolon-sep if multiple**, e.g.
  "Stanford University; University of California, San Francisco".

- funder:

  The funding agency. The relevant funder team will be made admin.

- initiative:

  Title of funding initiative, e.g. "Young Investigator Award".

- datasets:

  (Optional) List of datasets for which folders will be created under
  main data folder ("Raw Data"). Attributes set on the list items become
  annotations on the dataset folders.

- other_resources:

  (Optional) List of non-data resource types for which folders will be
  created. Attributes set on the list items become annotations on these
  folders.

- publicview:

  Whether to put this project in the public view instead of staying
  private (registered or non-registered users can see project).

- webview:

  Whether to open web browser to view newly created project. Defaults to
  FALSE.

- ...:

  Additional arguments. Not used.

## Value

The project object.

## Details

After project is created, NF Portal representation requires registration
in backend:

- New study row added to the Portal - Studies table.

- Project added to Portal - Files scope.
