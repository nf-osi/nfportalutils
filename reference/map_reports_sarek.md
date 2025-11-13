# Map out Sarek *report* files

This is in the family of helper funs to annotate secondary report files
from certain nextflow workflows. For Sarek, many report files are
conveniently outputted in a top-level "Reports" folder, which organize
reports by sample and then tool (BFCTools, FastQC, etc.). An example
reference starting at the sample-level:
https://www.synapse.org/#!Synapse:syn31665258 While most things in
"Reports" can indeed generally be called a "workflow report" (a subclass
of the "report" resource), some files in the bamQC reports directory are
misc web assets (.css, .js, .gif, etc.) used for the HTML report. HTML
reports *should* have these asset files are directly embedded/bundled in
the .html file, but when they don't these misc files become an extra
annotation burden. Since it's debatable to call something like a .css
file a report, these files are classified instead as "report asset".

## Usage

``` r
map_reports_sarek(syn_out, project)
```

## Arguments

- syn_out:

  The `Reports` output folder which is set as scope for fileview.

- project:

  Project in which to put fileview.

## Details

Unlike the other `map_*` functions, this requires a fileview instead of
using `walk` and will create one.
