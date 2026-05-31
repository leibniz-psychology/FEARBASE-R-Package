#' JSON Summary for a Named Object
#'
#' @description
#' Internal OpenCPU-oriented helper that converts a named data object visible to
#' `checkData()` into a JSON summary.
#'
#' @param d A single character string naming the object to summarize.
#'
#' @return A JSON string containing base summary output for each column.
#' @noRd
jsonSummary <- function(d) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to build JSON summaries.", call. = FALSE)
  }

  # Load the named object through checkData() so lookup validation is shared
  # with the rest of the OpenCPU-oriented helper surface.
  dat <- checkData(d)

  val <- jsonlite::toJSON(
    lapply(dat, function(x) {
      as.list(summary(x))
    }),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  return(val)
}
