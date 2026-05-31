#' Resolve the FEARBASE Codebook
#'
#' This internal helper centralizes the package convention for optional
#' codebook arguments. Explicit user input is preferred, then an interactive
#' caller-side object named `codebook`.
#'
#' @param cb A codebook data frame supplied by the caller, or `NULL`.
#' @param caller_env The environment to inspect for an object named `codebook`.
#'
#' @return A codebook data frame.
#' @noRd
.resolve_codebook <- function(cb = NULL, caller_env = parent.frame()) {
  ############################################################
  # 1) Prefer explicitly supplied data
  ############################################################

  # Explicit arguments make analyses reproducible and should always override
  # session objects or bundled convenience files.
  if (!is.null(cb)) {
    return(cb)
  }

  ############################################################
  # 2) Support interactive workflows with a caller-side codebook object
  ############################################################

  # Several package functions are used interactively with the FEARBASE data
  # objects already loaded into the workspace. Reuse such an object when it has
  # the rectangular structure needed by the codebook-based label helpers.
  if (exists("codebook", envir = caller_env, inherits = TRUE)) {
    codebook_candidate <- get(
      "codebook",
      envir = caller_env,
      inherits = TRUE
    )

    if (is.data.frame(codebook_candidate)) {
      return(codebook_candidate)
    }
  }

  ############################################################
  # 3) Fail clearly when no explicit or caller-side codebook exists
  ############################################################

  stop(
    "`cb` must be supplied, or an object named `codebook` must exist ",
    "in the calling environment.",
    call. = FALSE
  )
}

#' Build a Display-Label Lookup from the FEARBASE Codebook
#'
#' @param cb A codebook data frame.
#' @param attribute A single codebook attribute to select.
#' @param value_col Name of the output column containing abbreviations.
#' @param label_col Name of the output column containing display labels.
#' @param title_case Logical. If `TRUE`, convert labels to title case.
#'
#' @return A tibble with the requested abbreviation and label columns.
#' @noRd
.get_codebook_label_mapping <- function(
  cb,
  attribute,
  value_col,
  label_col,
  title_case = TRUE
) {
  ############################################################
  # 1) Validate the codebook contract used by all label helpers
  ############################################################

  .validate_data_frame(cb, "cb")
  .validate_required_columns(cb, c("attribute", "abbreviation", "name"), "cb")
  .validate_single_column_name(attribute, "attribute")
  .validate_single_column_name(value_col, "value_col")
  .validate_single_column_name(label_col, "label_col")
  .validate_logical_scalar(title_case, "title_case")

  ############################################################
  # 2) Select and normalize the requested codebook rows
  ############################################################

  # Keep a compact two-column lookup so joins remain explicit at call sites.
  # distinct() protects downstream joins from duplicated codebook rows.
  label_mapping <- cb |>
    filter(.data$attribute == attribute) |>
    select(
      "abbreviation",
      "name"
    ) |>
    filter(
      !is.na(.data$abbreviation),
      !is.na(.data$name)
    ) |>
    distinct()

  names(label_mapping) <- c(value_col, label_col)

  if (isTRUE(title_case)) {
    label_mapping[[label_col]] <- stringr::str_to_title(
      label_mapping[[label_col]]
    )
  }

  if (nrow(label_mapping) == 0L) {
    stop(
      "No `",
      attribute,
      "` rows with abbreviations and names were found in `cb`.",
      call. = FALSE
    )
  }

  return(label_mapping)
}
