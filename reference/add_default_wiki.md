# Add default wiki

Add the default wiki at project at creation or use to retrofit projects
where creators have not created a wiki.

## Usage

``` r
add_default_wiki(
  project,
  name,
  pi,
  lead,
  funder,
  initiative,
  abstract,
  institution
)
```

## Arguments

- project:

  Synapse id of project.

- name:

  Name of the project/study.

- pi:

  Name of the principal investigator.

- lead:

  Name(s) of the project lead/data coordinator, comma-sep if multiple,
  e.g. "Jane Doe, John Doe".

- funder:

  The funding agency. The relevant funder team will be made admin.

- initiative:

  Title of funding initiative, e.g. "Young Investigator Award".

- abstract:

  Project abstract/description.

- institution:

  Affiliated institution(s), **semicolon-sep if multiple**, e.g.
  "Stanford University; University of California, San Francisco".
