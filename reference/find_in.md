# Find in path

Get the Synapse id of an entity nested several folder layers deep
without having to click through the UI or create a fileview as long as
the structure/path is known.

## Usage

``` r
find_in(scope, path)
```

## Arguments

- scope:

  Id of the container (project or folder) to begin search.

- path:

  Path string in format "subdir1/subdir2/file.txt", where the last-level
  element will be the id returned.
