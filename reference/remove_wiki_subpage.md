# Remove a subpage from a project wiki

Removes a wiki subpage by name (header). Currently, this will decline to
make any mods if there is not exactly one match for the subpage. If
there are multiple subpages of same name, it's not clear which is the
right one to remove.

## Usage

``` r
remove_wiki_subpage(project_id, subpage)
```

## Arguments

- project_id:

  ID of the owner Synapse project.

- subpage:

  Name of the subpage
