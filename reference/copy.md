# Create copy of entity

Create a copy of syn entity; mostly used to create a copy on which to
test out changes. See
https://python-docs.synapse.org/build/html/synapseutils.html?highlight=copy#synapseutils.copy_functions.copy

## Usage

``` r
copy(
  entity,
  destination_id,
  skip_copy_wiki_page = FALSE,
  skip_copy_annotations = FALSE
)
```

## Arguments

- entity:

  Entity to copy.

- destination_id:

  Id of destination project/container that entity will be copied to.

- skip_copy_wiki_page:

  Whether to skip copying wiki; defaults FALSE.

- skip_copy_annotations:

  Whether to skip copying annotations; defaults FALSE.
