## load necessary packages

library(reticulate)

use_condaenv("r-reticulate", required = T) 

py_module_available("synapseclient") 
py_module_available("pandas")

library(nfportalutils)
library(dplyr)

## login, assuming creds are set with the following env vars
nfportalutils::syn_login(Sys.getenv("SYNAPSE_USER"), Sys.getenv("SYNAPSE_PWD"))

# Use an accessible sandbox project
# For this example, project is the "NF-dev" project
project_id <- "syn26452506"
subpage <- "Other databases"

label <- "Pubmed"
url <- "https://pubmed.ncbi.nlm.nih.gov/"
btn <- button_widget(label = label, url = url)

# Add button
wiki <- wiki_mod(btn, project_id, subpage = subpage, dry_run = FALSE)

# Remove button from subpage
wiki <- remove_button(wiki, label, dry_run = FALSE)

# Remove subpage entirely
wiki <- remove_wiki_subpage(project_id, subpage) 

# Add NF Data Curator App subpage
wiki <- data_curator_app_subpage(project_id, dry_run = FALSE)


