#' Validate a Data Frame Argument
#'
#' @param x Object to validate.
#' @param arg_name Name of the user-facing argument.
#'
#' @return Invisibly returns `x` when validation succeeds.
#' @noRd
.validate_data_frame <- function(x, arg_name) {
  if (!is.data.frame(x)) {
    stop("`", arg_name, "` must be a data frame.", call. = FALSE)
  }

  invisible(x)
}

#' Validate Required Data Frame Columns
#'
#' @param data Data frame whose columns should be checked.
#' @param required_cols Character vector of required column names.
#' @param arg_name Name of the user-facing data argument.
#'
#' @return Invisibly returns `data` when validation succeeds.
#' @noRd
.validate_required_columns <- function(data, required_cols, arg_name) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s) in `",
      arg_name,
      "`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(data)
}

#' Validate a Scalar Column Name
#'
#' @param x Value to validate.
#' @param arg_name Name of the user-facing argument.
#'
#' @return Invisibly returns `x` when validation succeeds.
#' @noRd
.validate_single_column_name <- function(x, arg_name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || identical(x, "")) {
    stop(
      "`",
      arg_name,
      "` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  invisible(x)
}

#' Validate a Scalar Logical Flag
#'
#' @param x Value to validate.
#' @param arg_name Name of the user-facing argument.
#'
#' @return Invisibly returns `x` when validation succeeds.
#' @noRd
.validate_logical_scalar <- function(x, arg_name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg_name, "` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  invisible(x)
}

#' Validate a Scalar Character Choice
#'
#' @param x Value to validate.
#' @param arg_name Name of the user-facing argument.
#' @param choices Supported values.
#'
#' @return The validated scalar value.
#' @noRd
.validate_choice <- function(x, arg_name, choices) {
  .validate_single_column_name(x, arg_name)

  if (!x %in% choices) {
    stop(
      "`",
      arg_name,
      "` must be one of: ",
      paste(choices, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  x
}

#' Coerce a Vector to Numeric with Data-Quality Validation
#'
#' @param x Vector to coerce.
#' @param value_label User-facing label for error messages.
#'
#' @return A numeric vector.
#' @noRd
.coerce_numeric_strict <- function(x, value_label) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  numeric_x <- suppressWarnings(as.numeric(as.character(x)))
  invalid_x <- !is.na(x) & is.na(numeric_x)

  if (any(invalid_x)) {
    stop(
      "All non-missing values in ",
      value_label,
      " must be numeric or coercible to numeric.",
      call. = FALSE
    )
  }

  numeric_x
}
