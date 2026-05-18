#' @title check data
#' @description
#' Helper function to check if the given dataset exists

createCsv <- function(file) {
  dataset <- read.csv(file)
  return(dataset)
}
