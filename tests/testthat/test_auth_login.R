test_that("Implicit login SYNAPSE_AUTH_TOKEN works", {
  skip_if_no_synapseclient()
  skip_if_no_token()
  withr::local_envvar(SYNAPSE_AUTH_TOKEN = Sys.getenv("TEST_SYNAPSE_AUTH_TOKEN"))
  # Testing for .syn in the global environment
  expect_is(syn_login(), "synapseclient.client.Synapse")
})