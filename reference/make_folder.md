# Create project folders

Use to set up a scaffold of standard upper-level folders as well as
customized data folders within "Raw Data" for a new project.

## Usage

``` r
make_folder(parent, folders)
```

## Arguments

- parent:

  The Synapse id or object that should be the parent container, i.e. the
  project or another folder.

- folders:

  List giving one or more folder names of folder(s) to create.

## Value

A list of the created folder object(s).

## Examples

``` r
if (FALSE) { # \dontrun{
datasets <- list("sequencing data", "imaging data")
assays <- c("rnaSeq", "immunohistochemistry")
for(i in seq_along(datasets)) attr(datasets[[i]], "assay") <- assays[[i]]
make_folder(parent = "syn26462036", datasets)
} # }
```
