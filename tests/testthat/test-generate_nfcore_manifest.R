test_that("generate_nfcore_manifest validates workflow_name", {
  skip_if_no_synapseclient()
  skip_if_no_token()
  
  expect_error(
    generate_nfcore_manifest("syn123", "invalid_workflow"),
    "workflow_name must be either 'rnaseq' or 'sarek'"
  )
})

test_that("generate_nfcore_manifest validates strandedness for rnaseq", {
  skip_if_no_synapseclient()
  skip_if_no_token()
  
  expect_error(
    generate_nfcore_manifest("syn123", "rnaseq", strandedness = "invalid"),
    "strandedness must be one of: 'forward', 'reverse', 'unstranded', 'auto'"
  )
})
