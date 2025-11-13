# Match to output-specific annotation function

This encodes logic for annotation and checks, e.g. DeepVariant is
Germline variant calling only, Mutect2 is for Somatic variant calling
only, while FreeBayes and Strelka2 can be applied to both; see
https://raw.githubusercontent.com/nf-core/sarek/3.4.2//docs/images/sarek_workflow.png

## Usage

``` r
annotation_rule(outputFrom, which = c("format_as", "annotate_as", "template"))
```
