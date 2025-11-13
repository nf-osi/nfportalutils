# Create default folders

A convenience wrapper around `make_folder` with NF defaults.

## Usage

``` r
add_default_folders(
  project,
  folders = c("Analysis", "Milestone Reports", "Raw Data")
)
```

## Arguments

- project:

  The project Synapse id or object.

- folders:

  Names of the standard set of folders.
