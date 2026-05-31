#' Retrieve an Object by Name with Strict Validation
#'
#' Retrieves an object by name from a specified environment with
#' strict input validation and structured error handling.
#'
#' @param d A character string of length one. The name of the object to retrieve.
#' @param envir An environment from which to retrieve the object.
#'   Defaults to the calling environment.
#' @param inherits Logical. Should enclosing environments be searched?
#'   Passed to \code{get()}. Defaults to \code{FALSE}.
#' @param warn_as_error Logical. If \code{TRUE}, warnings are converted to errors.
#'   Defaults to \code{TRUE}.
#'
#' @return The object named by \code{d}.
#'
#' @examples
#' x <- 1:5
#' check_data("x")
#'
#' \dontrun{
#' check_data("does_not_exist")
#' }
#'
#' @seealso \code{\link{get}}
#' @export
check_data <- function(d,
                      envir = parent.frame(),
                      inherits = FALSE,
                      warn_as_error = TRUE) {

  # ---- Input validation ----
  if (!is.character(d) || length(d) != 1L || is.na(d)) {
    stop("`d` must be a non-missing character string of length 1.",
         call. = FALSE)
  }

  if (!is.environment(envir)) {
    stop("`envir` must be a valid environment.",
         call. = FALSE)
  }

  if (!is.logical(inherits) || length(inherits) != 1L || is.na(inherits)) {
    stop("`inherits` must be a non-missing logical scalar.",
         call. = FALSE)
  }

  if (!is.logical(warn_as_error) || length(warn_as_error) != 1L || is.na(warn_as_error)) {
    stop("`warn_as_error` must be a non-missing logical scalar.",
         call. = FALSE)
  }

  # ---- Existence check (faster + clearer than tryCatch(get())) ----
  if (!exists(d, envir = envir, inherits = inherits)) {
    stop(sprintf("Object '%s' not found in the specified environment.", d),
         call. = FALSE)
  }

  # ---- Retrieval with optional warning escalation ----
  if (isTRUE(warn_as_error)) {

    val <- withCallingHandlers(
      get(d, envir = envir, inherits = inherits),
      warning = function(w) {
        stop(sprintf(
          "A warning occurred while retrieving object '%s': %s",
          d, conditionMessage(w)
        ), call. = FALSE)
      }
    )

  } else {

    val <- get(d, envir = envir, inherits = inherits)

  }

  return(val)
}

#' Read an Uploaded CSV File
#'
#' @description
#' OpenCPU-oriented helper that reads a CSV file path supplied by an upload
#' workflow and returns it as a data frame.
#'
#' @param file A single path to a CSV file.
#'
#' @return A data frame read from `file`.
#' @export
create_csv <- function(file) {
  # Use utils::read.csv() explicitly so the dependency is visible to package
  # checks while preserving the helper's historical base-R parsing behavior.
  dataset <- utils::read.csv(file)
  return(dataset)
}

#' JSON Summary for a Named Object
#'
#' @description
#' OpenCPU-oriented helper that converts a named data object visible to
#' `check_data()` into a JSON summary.
#'
#' @param d A single character string naming the object to summarize.
#' @param envir Environment from which to retrieve the named object. Defaults to
#'   the calling environment.
#' @param inherits Logical. Should enclosing environments be searched?
#'   Passed to \code{check_data()}. Defaults to \code{FALSE}.
#' @param warn_as_error Logical. If \code{TRUE}, warnings are converted to
#'   errors during object retrieval.
#'
#' @return A JSON string containing base summary output for each column.
#' @export
json_summary <- function(
  d,
  envir = parent.frame(),
  inherits = FALSE,
  warn_as_error = TRUE
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to build JSON summaries.", call. = FALSE)
  }

  # Load the named object through check_data() so lookup validation is shared
  # with the rest of the OpenCPU-oriented helper surface. The caller
  # environment is forwarded explicitly because the object name belongs to the
  # OpenCPU session or user workspace, not to this helper's local frame.
  dat <- check_data(
    d,
    envir = envir,
    inherits = inherits,
    warn_as_error = warn_as_error
  )

  val <- jsonlite::toJSON(
    lapply(dat, function(x) {
      as.list(summary(x))
    }),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  return(val)
}
