# Write meta file

Slightly different implementation than
https://github.com/Sage-Bionetworks/genie-erbb2-cbio/blob/develop/create_meta.R#L220

## Usage

``` r
write_meta(data, filename, publish_dir = ".", verbose = TRUE)
```

## Arguments

- data:

  The data (lines) to write.

- filename:

  Name of file.

- publish_dir:

  Directory path to write to, defaults to current.

- verbose:

  Report where file has been written.
