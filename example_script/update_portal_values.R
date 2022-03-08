## load necessary packages

library(reticulate)
reticulate::use_condaenv("r-reticulate", required = T)

py_available("synapseclient") #should be check in pckg...but here for now
py_available("pandas") #should be check in pckg...but here for now

library(nfportalutils)

study_tab_id <- 'syn16787123'
portal_fileview_id <- 'syn16858331'
people_tab_id <- 'syn23564971'

## login

nfportalutils::syn_login()

## update related studies column (dry run first)
clust <- nfportalutils::calculate_related_studies(study_tab_id, n_clust = 30)

nfportalutils::calculate_related_studies(study_tab_id, n_clust = 30, dry_run = F)

## update file annotations that can be derived from study table  (dry run first)
foo <- nfportalutils::update_study_annotations(study_table_id = study_tab_id,
                                               fileview_id = portal_fileview_id,
                                               annotations = c("studyId","studyName","fundingAgency","initiative"))

nfportalutils::update_study_annotations(study_table_id = study_tab_id,
                                               fileview_id = portal_fileview_id,
                                               annotations = c("studyId","studyName","fundingAgency","initiative"),
                                               dry_run = F)

## update study "data type" submmary values (dry run first)
data_types <- get_valid_values_from_json_schema()

foo <- assign_study_data_types(study_table_id = study_tab_id,
                        fileview_id = portal_fileview_id,
                        valid_values = data_types)

assign_study_data_types(study_table_id = study_tab_id,
                               fileview_id = portal_fileview_id,
                               valid_values = data_types,
                               dry_run = F)

## update people table (dry run first), using both the modifiedBy and createdBy columns
peeps <- nfportalutils::add_people_from_table(source_table_id = portal_fileview_id,
                                     source_column = "modifiedBy",
                                     people_table_id = people_tab_id,
                                     people_column = "ownerID")

peeps <- nfportalutils::add_people_from_table(source_table_id = portal_fileview_id,
                                              source_column = "modifiedBy",
                                              people_table_id = people_tab_id,
                                              people_column = "ownerID",
                                              dry_run = F)

peeps <- nfportalutils::add_people_from_table(source_table_id = portal_fileview_id,
                                              source_column = "createdBy",
                                              people_table_id = people_tab_id,
                                              people_column = "ownerID")

peeps <- nfportalutils::add_people_from_table(source_table_id = portal_fileview_id,
                                              source_column = "createdBy",
                                              people_table_id = people_tab_id,
                                              people_column = "ownerID",
                                              dry_run = F)
