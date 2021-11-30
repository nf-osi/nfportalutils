## load necessary packages

library(reticulate)

use_condaenv("r-reticulate", required = T) 

py_module_available("synapseclient") 
py_module_available("pandas")

library(nfportalutils)
library(dplyr)

## login, assuming creds are set with the following env vars
nfportalutils::syn_login(Sys.getenv("SYNAPSE_USER"), Sys.getenv("SYNAPSE_PWD"))   

## example to check the head set studies of the studies listed in portal studies table
study_tab_id <- 'syn16787123'
studies <- table_query(study_tab_id, "studyId") %>%
  pull(studyId)
study_subset <- head(studies)
results <- check_wiki_links(study_subset)

# view
results
# Write table for reference and additional manual review
# write.csv(results, "results.csv")