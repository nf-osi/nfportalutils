test_that("new_project_strict fails if clash with existing project", {
  skip_if_no_synapseclient() # does not require pandas
  skip_if_no_login()
  expect_error(new_project_strict("My project"))
})
