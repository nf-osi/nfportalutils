#' Send project ready notification email
#'
#' Adapts email template from https://sagebionetworks.jira.com/wiki/spaces/NPD/pages/2461925578/Project+Intake+Email+Templates
#' and fills in parameters from JSON configs (see configs stored at nf-osi/dcc-site) so that emails can be sent out programmatically.
#' To preview in RStudio, fill in the params and the email will show up in the Viewer pane.
#' To send, this requires an smtp creds file stored locally -- see `blastula::create_smtp_creds_file`.
#' 
#' @param config JSON config, from which `recipient` and `studyId` will be read.
#' @param send Send email. 
#' @param creds_file SMTP credentials file.
email_ready_notification <- function(config, send = TRUE, creds_file) {
  
  config <- read_study_config(config)
  recipient <- config$PI
  studyId <- config$studyId

  stopifnot(!is.null(recipient), !is.null(studyId))
  
  subject <- "REVIEW REQUESTED: Project ready on Synapse + Study listed on NF Data Portal"
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
  
  if(send) {
    blastula::smtp_send(
      email = email,
      to = recipient_email,
      from = "nf-osi@sagebionetworks.org",
      subject = subject,
      credentials = blastula::creds_file(creds_file)
  }
  return(email)
  
}


#' DSP-only request email (web form variant)
#' 
#' Originally, a request for Google Forms intake form + Google Docs DSP was sent in one email. 
#' Because of changes that obviate the need for the PI to fill in project info directly, and which has moved the DSP to a web form,
#' this request now uses the project config to direct a PI to the right location for filling out their DSP.  
#' 
#' @param config JSON config from which recipient name and address will be read.
email_dsp_request <- function(config) {
  
  config <- read_study_config(config)
  recipient <- config$PI
  fundingAgency <- config$fundingAgency
  NFID <- config$NFID
  project <- config$name
  
  # standard construction
  link <- glue::glue("https://dcc-site-dusky.vercel.app/dsp/{fundingAgency}?onfile={NFID}")
  
  email <- blastula::compose_email(
    body = blastula::md(glue::glue(
      "Dear {recipient}, 
      
      We're reaching out on behalf of Neurofibromatosis Open Science Initiative (NF-OSI) regarding **your new {fundingAgency}-funded project, {project}**. 
      We're excited to prepare your project on the [Synapse platform](https://www.synapse.org/) so that you can upload and share data for the NF community.
      In order to customize, track, and inform us of what support might be needed for your project, **please fill out a Data Sharing Plan** through this [web form]({link}). 
      
      If you have any questions or issues, please feel free to reach out nf-osi@sagebionetworks.org!
  
      Thanks,  
      NF-OSI Team"))
  )
  
  return(email)
}

