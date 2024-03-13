# REMOVE this once every functionality has switched to second login method below
test_that("(Legacy) Implicit login SYNAPSE_AUTH_TOKEN works", {
  skip_if_no_synapseclient()
  skip_if_no_token()
  withr::local_envvar(SYNAPSE_AUTH_TOKEN = Sys.getenv("TEST_SYNAPSE_AUTH_TOKEN"))
  # Testing for .syn in the global environment
  expect_is(syn_login(), "synapseclient.client.Synapse")
})


test_that("(synapser) Implicit authtoken login SYNAPSE_AUTH_TOKEN works", {
  skip_if_no_synapseclient()
  skip_if_no_token()
  withr::local_envvar(SYNAPSE_AUTH_TOKEN = Sys.getenv("TEST_SYNAPSE_AUTH_TOKEN"))
  testthat::expect_equal(synapser::synLogin(), NULL)
})
