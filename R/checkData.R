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
#'
#' @return The object named by \code{d}.
#'
#' @examples
#' x <- 1:5
#' checkData("x")
#'
#' \dontrun{
#' checkData("does_not_exist")
#' }
#'
#' @seealso \code{\link{get}}
#' @export
checkData <- function(d,
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
