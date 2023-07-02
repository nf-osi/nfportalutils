test_that("Update helper for item collection works with combined 'replace' and 'add' update types", {
  
  current <- list(
    list(entityId = "syn1", versionNumber = 1L),
    list(entityId = "syn2", versionNumber = 1L),
    list(entityId = "syn3", versionNumber = 1L)
  )
  
  update <- list(
    list(entityId = "syn3", versionNumber = 2L),
    list(entityId = "syn4", versionNumber = 2L),
    list(entityId = "syn5", versionNumber = 2L)
  )
  
  expected <- list(
    list(entityId = "syn1", versionNumber = 1L),
    list(entityId = "syn2", versionNumber = 1L),
    list(entityId = "syn3", versionNumber = 2L),
    list(entityId = "syn4", versionNumber = 2L),
    list(entityId = "syn5", versionNumber = 2L)
  )
  
  testthat::expect_identical(update_items(current, update),
                             expected)
  
})


test_that("Update helper for item collection works with just 'replace' update type", {
  
  current <- list(
    list(entityId = "syn1", versionNumber = 1L),
    list(entityId = "syn2", versionNumber = 1L),
    list(entityId = "syn3", versionNumber = 1L)
  )
  
  update <- list(
    list(entityId = "syn2", versionNumber = 2L),
    list(entityId = "syn3", versionNumber = 2L)
  )
  
  expected <- list(
    list(entityId = "syn1", versionNumber = 1L),
    list(entityId = "syn2", versionNumber = 2L),
    list(entityId = "syn3", versionNumber = 2L)
  )
  
  testthat::expect_identical(update_items(current, update),
                             expected)
  
})


test_that("Update helper for item collection works with just 'add' update type", {
  
  current <- list(
    list(entityId = "syn1", versionNumber = 1L),
    list(entityId = "syn2", versionNumber = 1L),
    list(entityId = "syn3", versionNumber = 1L)
  )
  
  update <- list(
    list(entityId = "syn4", versionNumber = 2L),
    list(entityId = "syn5", versionNumber = 2L)
  )
  
  expected <- list(
    list(entityId = "syn1", versionNumber = 1L),
    list(entityId = "syn2", versionNumber = 1L),
    list(entityId = "syn3", versionNumber = 1L),
    list(entityId = "syn4", versionNumber = 2L),
    list(entityId = "syn5", versionNumber = 2L)
  )
  
  testthat::expect_identical(update_items(current, update),
                             expected)
  
})
