#' Send project ready notification email
#'
#' Adapts email template from https://sagebionetworks.jira.com/wiki/spaces/NPD/pages/2461925578/Project+Intake+Email+Templates
#' and fills in parameters from JSON configs (see configs stored at nf-osi/dcc-site) so that emails can be sent out programmatically.
#' To preview in RStudio, fill in the params and the email will show up in the Viewer pane.
#' 
#' @param config JSON config, from which `recipient` and `studyId` will be read.
#' @param recipient Optional if `config` is provided, otherwise specify recipient name here.
#' @param studyId Optional if `config` is provided, otherwise the Synapse project id here.
email_ready_notification <- function(config, recipient = NULL, studyId = NULL) {
  
  if(!missing(config)) {
    config <- jsonlite::read_json(config)
    recipient <- config$PI
    studyId <- config$studyId
  } 
  stopifnot(!is.null(recipient), !is.null(studyId))
  
  projectHome <- glue::glue("https://www.synapse.org/#!Synapse:{studyId}")
  studyListing <- glue::glue("https://nf.synapse.org/Explore/Studies/DetailsPage/Details?studyId={studyId}")
  email <- blastula::compose_email(
      body = blastula::md(glue::glue(
        "Dear {recipient},

         This is a notification that your project is now ready on Synapse [here]({projectHome}). You should have received an email granting you access; if you did not, please let us know.
         Your project has also been [listed as this study]({studyListing}) on the [NF Data Portal](https://nf.synapse.org/). 
         Please review these pages to ensure the information is correct.
         
         A few reminders:
         - Data uploaded to your Synapse project remain private until the end of the embargo period (and we will contact you before making data available). 
         Until then, only your study’s summary and metadata are viewable on our portal (at the link above). 
         - If you are publishing a manuscript using data from your project, please let us know so that we can 1) help you mint a DOI for use in your publication, and 2) list your publication on the data portal.
         - Please let us know if your data upload plan (as outlined in your DSP) experiences major changes. Alerting us as soon as possible helps the curation team be prepared for data intake, metadata dictionary updates, etc.
         - Please review our [documentation](https://help.nf.synapse.org/NFdocs/) for the NF Data Portal to find step-by-step guides for uploading and annotating data, and feel free to reach out to nf-osi@sagebionetworks.org with any questions.
         

        Thanks again,  
        NF-OSI Team"))
    )
  return(email)
  
}

