#' Resolve Explicit or Caller-Side Data
#'
#' @param data Data frame supplied by the caller, or `NULL`.
#' @param arg_name Name of the user-facing data argument.
#' @param object_name Conventional object name to look up in `caller_env`.
#' @param caller_env Environment to inspect.
#'
#' @return A data frame supplied explicitly or found in `caller_env`.
#' @noRd
.resolve_caller_data <- function(
  data,
  arg_name,
  object_name,
  caller_env = parent.frame()
) {
  if (!is.null(data)) {
    return(data)
  }

  if (exists(object_name, envir = caller_env, inherits = TRUE)) {
    candidate <- get(object_name, envir = caller_env, inherits = TRUE)

    if (!is.data.frame(candidate)) {
      stop(
        "`",
        object_name,
        "` in the calling environment must be a data frame.",
        call. = FALSE
      )
    }

    return(candidate)
  }

  stop(
    "`",
    arg_name,
    "` must be supplied, or an object named `",
    object_name,
    "` must exist in the calling environment.",
    call. = FALSE
  )
}

#' Resolve Long-Format FEARBASE Data
#'
#' @param dl A long-format data frame supplied by the caller, or `NULL`.
#' @param caller_env Environment to inspect for `data_long`.
#'
#' @return A long-format data frame.
#' @noRd
.resolve_long_data <- function(dl = NULL, caller_env = parent.frame()) {
  .resolve_caller_data(
    data = dl,
    arg_name = "dl",
    object_name = "data_long",
    caller_env = caller_env
  )
}

#' Resolve FEARBASE Metadata
#'
#' @param md A metadata data frame supplied by the caller, or `NULL`.
#' @param caller_env Environment to inspect for `metadata`.
#'
#' @return A metadata data frame.
#' @noRd
.resolve_metadata <- function(md = NULL, caller_env = parent.frame()) {
  .resolve_caller_data(
    data = md,
    arg_name = "md",
    object_name = "metadata",
    caller_env = caller_env
  )
}
