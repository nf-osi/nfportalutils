# Validation workflows

This vignette demonstrates two approaches for programmatic metadata
validation workflows using the utilities in this package. Validating
metadata via the UI is covered elsewhere.

First set up as usual:

``` r
library(nfportalutils)
syn_login(Sys.getenv("SYNAPSE_AUTH_TOKEN"))
```

## Option 1: Using Synapse validation service

The Synapse validation service will be the main validation service to
use starting in 2026. Before validating with Synapse’s native backend
capabilities, make sure a JSON schema is “bound” to the folder. Here’s
our example folder with several files; two directly under the folder and
one in a subfolder.

``` r
my_dataset <- "syn51106460"
```

1.  Bind the schema that we know to be the appropriate one:

``` r
# Preferred: Bind explicit version
bind_schema(id = my_dataset, schema_id = "org.synapse.nf-generalmeasuredatatemplate-10.2.0")

# Use carefully: Bind the "latest" (last registered) version
# bind_schema(id = my_dataset, schema_id = "org.synapse.nf-generalmeasuredatatemplate")
```

2.  Validate all items in the folder. A fileview that includes the
    folder in the scope is required. The results are returned as the
    “raw” response; if you want a summary format, create a function to
    process the results as desired.

``` r
validate_dataset_folder(my_dataset, fileview = "syn64364141")
```

## Option 2 (will be DEPRECATED in 2026): Using Schematic API service

Most validation will use schematic service until 2026. Note that the
schematic API only works with dataset folders currently. An example
dataset folder is given below:

``` r
my_dataset <- "syn25386362"
```

1.  To validate metadata, a manifest must be reconstituted, i.e. we
    create a csv from the annotations currently on the files. Type
    [`?manifest_generate`](../reference/manifest_generate.md) to read
    the docs.

You need to know the `data_type` to validate against. The `data_type` is
the same as the “Component” value in the schematic data model. If
feeling lucky, try `infer_data_type`.

``` r
inferred <- infer_data_type(my_dataset)
inferred
```

Once `data_type` is in hand, here’s the general invocation:

``` r
data_type <- inferred$data_type # Or set manually otherwise

manifest_generate(data_type, 
                  dataset_id = my_dataset,
                  schema_url = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                  output_format = "google_sheet") # otherwise excel 
```

2.  Go to the google_sheet and download as `.csv`. If Excel was chosen,
    open in some spreadsheet editor and resave file as `.csv`. Then
    validate.

``` r
manifest_validate(data_type = data_type, 
                  file_name = "GenomicsAssayTemplate - Sheet1.csv")
```

3.  Make corrections in the `.csv` according to validation laundry list.

4.  Submit corrected manifest via DCA (or via `annotate_with_manifest`).
