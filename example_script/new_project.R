library(nfportalutils)

# SYNAPSE_AUTH_TOKEN should be set
nfportalutils::syn_login()

project <- new_project(name = "NF Service Test Project", 
            pi = "Jane Doe", 
            lead = "My Post-Doc", 
            abstract = "# The Project ",
            institution = "Great Institution",
            funder = "CTF",
            initiative = "First Intiative",
            webview = TRUE)

# Delete project
project <- .syn$delete(project)


# Fails because there is a project named "test project"
# https://www.synapse.org/#!Synapse:syn297093
# You lack CHANGE_PERMISSIONS access to the requested entity.
new_project(name = "test project", 
            pi = "Jane Doe", 
            lead = "My Post-Doc", 
            abstract = "# The Project",
            institution = "Great Institution",
            funder = "CTF",
            initiative = "First Intiative",
            webview = TRUE)
