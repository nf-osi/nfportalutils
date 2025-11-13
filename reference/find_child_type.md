# Find children of type

Small utility like `find_child` but retrieves files by type rather than
by specific name. Returns a vector of ids, with entity names set as
names.

## Usage

``` r
find_child_type(parent, child_type = list("file"))
```

## Arguments

- parent:

  Parent container (project or folder).

- child_type:

  Type(s) as a list, even for only one type. Defaults to "file".
