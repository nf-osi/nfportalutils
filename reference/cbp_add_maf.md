# Export and add mutations data to cBioPortal dataset

This should be run in an existing dataset package root.

## Usage

``` r
cbp_add_maf(maf_data, verbose = TRUE)
```

## Arguments

- maf_data:

  Synapse id of `merged maf` file for public release.

- verbose:

  Whether to be chatty.

## Details

Get merged maf file that represents filtered subset of `maf`s containing
only (non-germline) data OK to release publicly. This needs to be
packaged with other files like this [example of a public mutations
dataset](https://github.com/cBioPortal/datahub/tree/1e03ea6ab5e0ddd497ecf349cbee7d50aeebcd5e/public/msk_ch_2020).
