# Package index

## Table Utils

### Main PORTAL table data update and management

Augment and update one of the main portal tables (e.g. Portal - Studies)

- [`add_publication_from_pubmed()`](add_publication_from_pubmed.md) :
  Add a publication to the publication table
- [`add_publication_from_unpaywall()`](add_publication_from_unpaywall.md)
  : Add a publication or preprint to the publication table via the
  Unpaywall API.
- [`add_publications_from_file()`](add_publications_from_file.md) : Add
  a batch of publications from spreadsheet
- [`assign_study_data_types()`](assign_study_data_types.md) : Summarize
  data types for the study
- [`calculate_related_studies()`](calculate_related_studies.md) :
  Calculate and add related studies to study table
- [`add_people_from_table()`](add_people_from_table.md) : Update the
  People table from a source Table or View column
- [`summarize_attribute()`](summarize_attribute.md) : Helper
  summarization util

### Lower-level table maintenance

- [`adjust_view()`](adjust_view.md) : Adjust view
- [`swap_col()`](swap_col.md) : Swap out old column for a new column in
  a schema
- [`byte_budget()`](byte_budget.md) : Calculate byte budget for a schema

## Project Configuration and Management

Create or retrofit an NF project to expected default structure and
assets

- [`add_default_fileview()`](add_default_fileview.md) : Create default
  project fileview
- [`add_default_folders()`](add_default_folders.md) : Create default
  folders
- [`make_admin()`](make_admin.md) : Make a user or group full admin of a
  Synapse entity

## Metadata Utils

### General annotations

Add and manage annotations on Synapse entities

- [`set_annotations()`](set_annotations.md) :

  Wrapper around the Python `set_annotations` that pulls current
  annotations and adds new annotations with given annotations data or
  replaces data for annotations with the same keys existing on the
  entity.

- [`update_study_annotations()`](update_study_annotations.md) : Updates
  a set of files with project-level annotations.

- [`annotate_with_manifest()`](annotate_with_manifest.md) : Set
  annotations from a manifest

- [`copy_annotations()`](copy_annotations.md) : Copy annotations

- [`.modify_annotation()`](dot-modify_annotation.md) : Modify a single
  annotation on a single file

- [`meta_qc_dataset()`](meta_qc_dataset.md) : QC dataset metadata with
  pass/fail result

- [`meta_qc_project()`](meta_qc_project.md) : QC metadata at the project
  level with pass/fail result

- [`manifest_generate()`](manifest_generate.md) : Generate manifest via
  schematic service

- [`manifest_validate()`](manifest_validate.md) : Validate manifest via
  schematic service

- [`manifest_validate_wrapper()`](manifest_validate_wrapper.md) :
  Validate with stated data_type in manifest

- [`manifest_passed()`](manifest_passed.md) : Provide a pass/fail
  summary result

- [`precheck_manifest()`](precheck_manifest.md) : Precheck a manifest

- [`remanifest()`](remanifest.md) : Reconstitute a manifest

- [`infer_data_type()`](infer_data_type.md) : Infer data type of a
  dataset folder

- [`generate_nfcore_manifest()`](generate_nfcore_manifest.md) : Generate
  nf-core samplesheet manifest

### Special annotation of nextflow processed data

Special annotation of nextflow processed data

- [`map_reports_sarek()`](map_reports_sarek.md) :

  Map out Sarek *report* files

- [`map_sample_input_ss()`](map_sample_input_ss.md) : Parse nextflow
  samplesheet for sample inputs

- [`map_sample_io()`](map_sample_io.md) : Map sample input-output

- [`map_sample_output_rnaseq()`](map_sample_output_rnaseq.md) : Map
  sample to output from nf-rnaseq with path

- [`map_sample_output_sarek()`](map_sample_output_sarek.md) : Map sample
  to output from nf-sarek

- [`annotate_nf_workflow()`](annotate_nf_workflow.md) : Annotate
  nextflow workflow outputs

- [`annotate_processed()`](annotate_processed.md) : Annotate processed
  data

- [`annotate_aligned_reads()`](annotate_aligned_reads.md) : Annotate
  processed aligned reads

- [`annotate_called_variants()`](annotate_called_variants.md) : Annotate
  somatic or germline variants output

- [`annotate_quantified_expression()`](annotate_quantified_expression.md)
  : Annotate quantified expression output

- [`annotate_reports_sarek()`](annotate_reports_sarek.md) : Annotate
  Sarek reports

- [`annotate_with_samtools_stats()`](annotate_with_samtools_stats.md) :
  Make annotations from samtools stats

- [`processed_meta()`](processed_meta.md) : Metadata for processed
  products

- [`nf_workflow_version()`](nf_workflow_version.md) : Return workflow
  version according to workflow meta

### Validate metadata

- [`validate_bound_entity()`](validate_bound_entity.md) :

  Validate a **schema-bound** entity

- [`validate_dataset_folder()`](validate_dataset_folder.md) : Validate
  dataset folder

- [`validate_collection_items()`](validate_collection_items.md)
  [`validate_dataset_items()`](validate_collection_items.md) : Validate
  metadata of items in a collection

## Dataset Creation and Management

### General dataset creation and citation

Create datasets in general

- [`new_dataset()`](new_dataset.md) : Create new dataset with given
  items

### Working with dataset collections to manage datasets after creation

- [`add_to_collection()`](add_to_collection.md) : Add to collection
- [`use_latest_in_collection()`](use_latest_in_collection.md) : Update
  item versions to "latest" in a collection
- [`update_items()`](update_items.md) : INTERNAL - apply updates to a
  collection of items

## Data Model Utils

Talk to a JSON-LD data model important to the portal data
(i.e. NF-metadata-dictionary)

- [`get_by_prop_from_json_schema()`](get_by_prop_from_json_schema.md) :
  Look up connected nodes by specified property in JSON-LD schema
- [`get_dependency_from_json_schema()`](get_dependency_from_json_schema.md)
  : Get dependencies for node in JSON-LD schema
- [`get_valid_values_from_json_schema()`](get_valid_values_from_json_schema.md)
  : Retrieve valid subclasses of a value in a JSON-LD schema
- [`key_label_to_id()`](key_label_to_id.md) : Query for schema key id
  given label
- [`schema_max_str_len()`](schema_max_str_len.md) : Consult schema about
  max string length

## Governance Utils

Analyze and manage data access/restrictions

- [`make_public_viewable()`](make_public_viewable.md) : Set public
  access to VIEW (READ) only for an entity
- [`make_public()`](make_public.md) : Make public
- [`check_access()`](check_access.md) : Check access
- [`summarize_file_access()`](summarize_file_access.md) : Summarize file
  access for files within some view
- [`grant_specific_file_access()`](grant_specific_file_access.md) :
  Provide access to a specific set of files using a query result.

## Search Utils

Help locate Synapse accessions, etc.

- [`find_child()`](find_child.md) : Find id of a child entity in a
  container
- [`find_child_type()`](find_child_type.md) : Find children of type
- [`find_data_root()`](find_data_root.md) : Find data folder
- [`find_in()`](find_in.md) : Find in path
- [`find_nf_asset()`](find_nf_asset.md) : Find a standard nextflow
  workflow output asset
- [`find_parent()`](find_parent.md) : Find parent

## Provenance Utils

Manage provenance metadata

- [`add_activity()`](add_activity.md) : Add activity to entity
- [`add_activity_batch()`](add_activity_batch.md) : Add activity to
  multiple entities
- [`delete_provenance()`](delete_provenance.md) : Remove provenance info

## Content Utils

Create and manage content for projects and pages

- [`add_default_wiki()`](add_default_wiki.md) : Add default wiki
- [`wiki_mod()`](wiki_mod.md) : Add markup to a project wiki
- [`remove_wiki_subpage()`](remove_wiki_subpage.md) : Remove a subpage
  from a project wiki
- [`data_curator_app_subpage()`](data_curator_app_subpage.md) : Create
  NF Data Curator App subpage
- [`get_project_wiki()`](get_project_wiki.md) : Get wiki content of
  synapse project(s)
- [`check_wiki_links()`](check_wiki_links.md) : Check wiki links
- [`remove_button()`](remove_button.md) : Remove button from a project
  wiki

### Figures and diagrams

Supplemental figures and diagrams that go into Wikis or other places

- [`processing_flowchart()`](processing_flowchart.md) : Wrapper to
  create data-driven flowchart with pretty processing provenance mermaid
  template
- [`dsp_dataset_mapping()`](dsp_dataset_mapping.md) : Wrapper to create
  Data Sharing Plan to project dataset comparison chart
- [`bipartite_mmd_template()`](bipartite_mmd_template.md) : Simple
  bipartite representation in mermaid charts

## Export Data to Other Platforms

Helpers to export/release NF data to other platforms/databases.

### cBioPortal

Export data as a cBioPortal study

- [`cbp_new_study()`](cbp_new_study.md) : Initialize a new cBioPortal
  study dataset
- [`cbp_new_cancer_type()`](cbp_new_cancer_type.md) : Create reference
  file for new cancer type
- [`cbp_add_maf()`](cbp_add_maf.md) : Export and add mutations data to
  cBioPortal dataset
- [`cbp_add_clinical()`](cbp_add_clinical.md) : Export and add clinical
  data to cBioPortal dataset
- [`cbp_add_expression()`](cbp_add_expression.md) : Export and add
  expression data to cBioPortal dataset
- [`cbp_add_cna()`](cbp_add_cna.md) : Export and add CNA (seg) data to
  cBioPortal dataset

## Quality Control and Testing Utils

QC data

- [`check_readpair_validity()`](check_readpair_validity.md) : Check
  fastq read pair matches samplesheet read pair assignment.
- [`identify_read_pair()`](identify_read_pair.md) : Identify read pair
  from string
- [`test_failed()`](test_failed.md) : Format a test fail message.
- [`test_passed()`](test_passed.md) : Format a test passed message.

## Basic Utils

Low-level functions

- [`syn_login()`](syn_login.md) : Logs into Synapse.
- [`table_query()`](table_query.md) : Generic table query
- [`as_table_schema()`](as_table_schema.md) : Transform table data to
  target schema for Synapse storage
- [`make_folder()`](make_folder.md) : Create project folders
- [`bind_schema()`](bind_schema.md) : Wrapper for JSON schema binding
- [`add_to_scope()`](add_to_scope.md) : Add to scope
- [`new_view()`](new_view.md) : Create a view
- [`list_project_datasets()`](list_project_datasets.md) : List datasets
  in project
- [`latest_version()`](latest_version.md) : Get the latest version
- [`walk()`](walk.md) : Walk through a directory
- [`copy()`](copy.md) : Create copy of entity
- [`get_path()`](get_path.md) : Get path for a Synapse id
- [`convert_to_stringlist()`](convert_to_stringlist.md) : Convert a
  delimited string to a stringlist annotation
- [`bare_syn_id()`](bare_syn_id.md) : Extract synapse id from URI or
  other string
- [`bad_url()`](bad_url.md) : Helper function to check urls
- [`.update_table_data()`](dot-update_table_data.md) : Replace/update
  table contents = input data must have ROW_ID and ROW_VERSION columns
  to update, otherwise will append data.
- [`.update_view_data()`](dot-update_view_data.md) : Replace/update
  table contents = input data must have ROW_ID, ROW_VERSION, ETAG
  columns to update.
- [`from_pubmed()`](from_pubmed.md) : Get publication metadata from
  PubMed

## Internal/experimental

Mostly meant to be internal or experimental stuff

- [`.delim_string_to_vector()`](dot-delim_string_to_vector.md) : Convert
  a delimited string to vector, utility function.
- [`.dict_to_list()`](dot-dict_to_list.md) : Convert a flat Python Dict
  to R list
- [`.replace_string_column_with_stringlist_column()`](dot-replace_string_column_with_stringlist_column.md)
  : Replace string column with stringlist column
- [`.store_rows()`](dot-store_rows.md) : Adds a row to a table.
- [`missing_annotation_email()`](missing_annotation_email.md) : Convert
  a delimited string to a stringlist annotation
- [`get_doi_meta()`](get_doi_meta.md) : Get DOI metadata if it exists
- [`cite_dataset()`](cite_dataset.md) : Generate example dataset
  citation
