# Map sample to output from nf-rnaseq with path

See https://nf-co.re/rnaseq/docs/output/#pipeline-overview. The workflow
generates several types of outputs that can become ("level 2" or "level
3") dataset products. (note: some outputs may not be available depending
on how workflow was run and indexed back).

## Usage

``` r
map_sample_output_rnaseq(
  syn_out,
  fileview,
  output = c("STAR and Salmon", "featureCounts", "SAMtools")
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

  Which output to select and annotate. Defaults to all output types
  unless more limited election.

  - "STAR and Salmon" selects .sf files – this is typically considered
    the main output.

  - "featureCounts" selects the relevant .txt files.

  - "SAMtools" selects the .bam/.bai indexed and sorted by SAMtools.

## Value

A list of `data.table`s with columns `output_name` `output_id` `sample`
`workflow` for each output type. An attribute `workflow=nf-rnaseq` will
be set on the returned list, and elements will have attribute
`outputFrom` set, e.g. `outputFrom=SAMtools`.
