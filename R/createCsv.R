#' Read an Uploaded CSV File
#'
#' @description
#' Internal OpenCPU-oriented helper that reads a CSV file path supplied by an
#' upload workflow and returns it as a data frame.
#'
#' @param file A single path to a CSV file.
#'
#' @return A data frame read from `file`.
#' @noRd
createCsv <- function(file) {
  # Use utils::read.csv() explicitly so the dependency is visible to package
  # checks while preserving the helper's historical base-R parsing behavior.
  dataset <- utils::read.csv(file)
  return(dataset)
}
