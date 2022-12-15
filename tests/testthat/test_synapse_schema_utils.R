# Test fixture is table syn49378540 on Synapse, with schema that includes INTEGER and STRING and STRING_LIST columns with character size and character + list size limits.
# These tests mainly focus on the character and list validation and JSON encoding functionality that `as_table_schema` provides
# in order to successfully append data conforming to an existing Synapse schema

test_that("`as_table_schema` works as expected for data that can be fit into schema without issue", {
  test_fixture <- "syn49378540"
  test_data <- data.frame(Movie = "The Sound of Music",
                          Year = 1965,
                          Favorites = I(list(list("raindrops","whiskers", "kettles"))))
  test_data_storable <- as_table_schema(test_data, schema = test_fixture)
  testthat::expect_s3_class(.syn$store(test_data_storable), "synapseclient.table.CsvFileTable")
})

test_that("`as_table_schema` errors for data with missing column", {
  test_fixture <- "syn49378540"
  test_data <- data.frame(Movie = "The Sound of Music",
                          Year = 1965)
  testthat::expect_error(as_table_schema(test_data, schema = test_fixture))
  
})


test_that("`as_table_schema` errors for data that exceeds list length as specified in schema for LIST column and truncation is not allowed", {
  test_fixture <- "syn49378540"
  test_data <- data.frame(Movie = "The Sound of Music",
                          Year = 1965,
                          Favorites = I(list(list("raindrops", "whiskers", "kettles", "mittens")))) # exceeds list length of 3
  testthat::expect_error(as_table_schema(test_data, schema = test_fixture, list_truncate = FALSE))
  
})


test_that("`as_table_schema` returns result with warning for data that exceeds list length specified in schema for LIST column but truncated to fit", {
  test_fixture <- "syn49378540"
  test_data <- data.frame(Movie = "The Sound of Music",
                          Year = 1965,
                          Favorites = I(list(list("raindrops", "whiskers", "kettles", "mittens")))) # exceeds list length of 3
  testthat::expect_warning(as_table_schema(test_data, schema = test_fixture, list_truncate = TRUE))
  
})


test_that("`as_table_schema` errors for data that exceeds character limits specified in schema for STRING_LIST column", {
  test_fixture <- "syn49378540"
  test_data <- data.frame(Movie = "The Sound of Music",
                          Year = 1965,
                          Favorites = I(list(list("raindrops on roses", "whiskers")))) # exceeds character size of 15 for first value
  testthat::expect_error(as_table_schema(test_data, schema = test_fixture))
  
})

