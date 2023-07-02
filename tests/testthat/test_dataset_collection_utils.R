test_that("Update helper for dataset collection items works with combined replace/add updates", {
  
  current_items <- list(
    list(entityId = "syn1", versionNumber = 1L),
    list(entityId = "syn2", versionNumber = 1L),
    list(entityId = "syn3", versionNumber = 1L)
  )
  
  update_items <- list(
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
  
  testthat::expect_equal(update_items(current_items, update_items),
                         expected)
  
})




