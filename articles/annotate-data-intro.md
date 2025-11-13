# Introduction to utils for annotating data

## Intro

This introduces the annotation utilities with typical examples. This is
expected to be the more useful starting point for using nfportalutils
for annotation tasks, to be followed by the more specialized vignetted
for annotating NF processed data if needed.

### Set up

``` r
library(nfportalutils)

syn_login()

# Change this to a dev project you have access to  
PROJECT <- "syn26462036"
```

### Set annotations on a single file

Create a demo entity.

``` r

synapseclient <- reticulate::import("synapseclient")
# Create an entity with some initial annotations
entity <- synapseclient$Folder("Demo Entity",
                               parent = PROJECT,
                               annotations = list(foo = "bar", favorites = c("raindrops", "whiskers")))

entity <- .syn$store(entity)
```

`set_annotations` can be used to add new annotations or correct an
existing annotation on an entity. This wraps the Python client to make
it more intuitive to pass in an R list as the annotations as above.
Here, add another annotation *and* correct the `favorites` to
“chocolate”. The returned data shows the unchanged `foo`, the updated
`favorites`, and a new `n`.

``` r
set_annotations(id = entity$properties$id, annotations = list(favorites = "chocolate", n = 7L))
```

Cleanup.

``` r
.syn$delete(entity)
```

### Annotate in batch using a manifest

A better way to use `set_annotations` for a set of entities, usually
files.

First create multiple entities that need to be annotated or corrected in
batch.

``` r
objs <- make_folder(parent = PARENT_TEST_PROJECT, folders = c("mock_file_1", "mock_file_2", "mock_file_3"))
ids <- sapply(objs, function(x) x$properties$id)
```

Create example manifest. Note: Another way includes reading in a
shematic csv manifest with entityIds and Filenames.

``` r
manifest <- data.table(
     entityId = ids,
     assay = "drugScreen",
     experimentalTimepoint = c(1L, 3L, 7L),
     experimentalTimepointUnit = "days",
     cellType = list(c("schwann", "macrophage"), c("schwann", "macrophage"), c("schwann", "macrophage"))
   )
manifest
```

Apply:

``` r
annotate_with_manifest(manifest)
```

Cleanup.

``` r
for (id in ids) .syn$delete(id)
```
