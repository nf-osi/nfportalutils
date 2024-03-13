# Functions especially useful for tutorials or testing

#' Create n temp files 
#' 
#' Create some text files for upload
#' 
#' @param n Integer number of files.
#' @return Paths to files in the temp directory.
mock_files <- function(n) {
  paths <- vector(mode = "character", length = n)
  for(i in 1:n) {
    paths[i] <- filePath <- tempfile("data_", fileext = c(".txt"))
    writeLines(text = sample(letters, 10), con = filePath)
  }
  paths
}