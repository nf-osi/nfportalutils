# Reconstitute a manifest

Quickly "reconstitute" a manifest for files with existing annotations.
This is a *power-user* alternative to schematic's `manifest_generate`
with option `useAnnotations = TRUE` for certain scenarios:

## Usage

``` r
remanifest(scope, file = "manifest.csv", recurse = TRUE, mf = data.table())
```

## Arguments

- scope:

  Synapse id of something representing one or more datasets. Can be a
  folder *or* Synapse dataset entity.

- file:

  Name of file to be written. Use `NULL` to return the `data.table`
  instead of writing to file.

- recurse:

  Include everything nested within folder scope, default `TRUE`, or only
  the immediate files. Not used if dataset is Synapse dataset entity.

- mf:

  Intermediate manifest to build upon; used when recursive.

## Details

- Meant to be faster and more convenient for workflows focused on
  scalable/programmatic annotation/revalidation. Convenience and speed
  mainly comes from bypassing the schematic API layer and using the
  Synapse API directly, so manifest is reconstituted directly as R
  `data.table` **instead of having to download a Googlesheets or Excel
  intermediate**. **The result `.csv` is still intended to be
  schematic-compatible for validation via `manifest_validate`**.

- Works with both a Synapse dataset entity as well as a folder;
  schematic currently only handles the latter (the folder concept
  predates Synapse datasets).

- If folder-scoped, can choose between generating manifest with just
  immediate files *or*, like schematic, nested files. This is useful,
  for example, if the main scope folder is "RNA-seq" and the main files
  are fastqs, but there is a child folder called "md5" with md5 files,
  and the manifest is really meant for the fastqs.

- Does not try to download `synapse_manifest.csv`, which can be useful
  to regenerate a manifest when ARs have been set.

- Does not require an asset view.

Other developer notes: See also `gather_annotations` for the
under-the-hood workhorse. Intended for further development to be used as
part of advanced annotation utils.
