test_that("Add study meta works", {
  skip_if_no_synapseclient()
  skip_if_no_login()

  study_meta <- list(
    "studyName" = "NF Dev Playground",
    "fundingAgency" = "Sage Bionetworks",
    "initiative" = "Other",
    "studyLeads" = c("Robert Allaway", "Anh Nguyet Vu"),
    "institutions" = "Sage Bionetworks",
    "diseaseFocus" = "Multiple",
    # "manifestation" # OK for missing manifestation
    # "studyStatus" = "Active" # expect automatically filled "Active" if missing
    "dataStatus" = "Data Not Expected"
  )

  s <- add_new_study_meta(
    id = "syn26462036",
    study_meta = study_meta)

  expected <- c(study_meta, studyStatus = "Active")

  testthat::expect_output(print(s), "\\{'studyName': \\['NF Dev Playground'\\], 'dataStatus': \\['Data Not Expected'\\], 'initiative': \\['Other'\\], 'studyLeads': \\['Robert Allaway', 'Anh Nguyet Vu'\\], 'studyStatus': \\['Active'\\], 'diseaseFocus': \\['Multiple'\\], 'institutions': \\['Sage Bionetworks'\\], 'fundingAgency': \\['Sage Bionetworks'\\]\\}")

})


# register_study(id = "syn26462036", study_meta = study_meta, summary = paste("Test registration on", Sys.Date()), study_summary_table = "syn27353709")
