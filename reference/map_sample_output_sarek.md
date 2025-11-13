# Map sample to output from nf-sarek

See https://nf-co.re/sarek. Similar to
[`map_sample_output_rnaseq`](map_sample_output_rnaseq.md) but for Sarek
outputs. Processed outputs have been seen to have variable organization,
nested first by sample or by caller as
`VariantCalling/<SAMPLE>/<CALLER>` or as
`VariantCalling/<CALLER>/<SAMPLE>`. The hierarchy/context needed for
annotation can be obtained with a fileview and given the necessary
starting point `syn_out`, which is the id of the relevant output folder
(called `VariantCalling` or `variant_calling` usually).

## Usage

``` r
map_sample_output_sarek(
  syn_out,
  fileview,
  output = c("CNVkit", "DeepVariant", "Strelka2", "Mutect2", "FreeBayes")
)
```

## Arguments

- syn_out:

  Syn id of variant calling output folder.

- fileview:

  An existing fileview to use (usually the project's local fileview)
  that scopes outputs and has "default" columns (`id`, `name`, `type`,
  `parentId`, `path`, ...). See details.

- output:

  Which output to select and annotate. Defaults to checking the presence
  of possible prioritized outputs ("CNVkit", "DeepVariant", "Strelka2",
  "Mutect2", and "FreeBayes"), though typically only a subset makes
  sense and can be explicitly specified, which also speeds up results.

## Value

A `data.table` with cols `caller` `path` `output_name` `output_id`
`sample` `workflow`. An attribute `workflow=nf-sarek` will be set on the
returned list, and elements will have attribute `outputFrom` set, e.g.
`outputFrom=CNVkit`.

## Details

Note: Depending on when `map_sample_output_sarek` is run on the output
directory, there may be just `vcf` or both `vcf` and `maf` present.
**`maf` are ignored since technically it is not an output of sarek but
from further processing with nf-vcf2maf.**
