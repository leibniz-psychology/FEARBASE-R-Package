#' Resolve Long-Format Data for Sample-Size Helpers
#'
#' This internal helper keeps the public sample-size functions aligned on how
#' omitted data are resolved. It first looks for a caller-side object named
#' `data_long`, then falls back to the package-bundled `data/data_long.csv`
#' file when that file is available.
#'
#' @param dl A data frame supplied by the caller, or `NULL`.
#'
#' @return A data frame in long format.
#' @noRd
.resolve_sample_size_long_data <- function(dl = NULL) {
  ############################################################
  # 1) Return explicitly supplied data without side effects
  ############################################################

  # Explicit arguments are the most reproducible source of data, so they take
  # precedence over session objects or package-bundled convenience data.
  if (!is.null(dl)) {
    return(dl)
  }

  ############################################################
  # 2) Look for an interactive caller-side data_long object
  ############################################################

  # Preserve the package's existing interactive behavior: several helpers and
  # tests call sample-size functions without a data argument and expect an
  # object named `data_long` to be used when it is present.
  if (exists("data_long", envir = parent.frame(), inherits = TRUE)) {
    data_long_candidate <- get(
      "data_long",
      envir = parent.frame(),
      inherits = TRUE
    )

    # Use a caller-side data_long object only when it already has the minimum
    # long-format columns needed by the sample-size helpers. This avoids using
    # malformed lazy-data objects created from raw CSV files in package checks.
    if (
      is.data.frame(data_long_candidate) &&
        all(c("study_id", "participant_id") %in% names(data_long_candidate))
    ) {
      return(data_long_candidate)
    }

    # Raw CSV files stored in an R package `data/` directory can be exposed by
    # lazy loading as a one-column data frame whose column name contains the
    # original comma-separated header with dots substituted for commas. Detect
    # that narrow shape and parse it back into a regular rectangular data frame
    # so zero-argument calls continue to work in installed-package checks.
    if (
      is.data.frame(data_long_candidate) &&
        ncol(data_long_candidate) == 1L &&
        grepl("study_id.*participant_id", names(data_long_candidate)[1])
    ) {
      data_long_lines <- as.character(data_long_candidate[[1]])

      # The sample-size helpers only require the first two CSV fields. Extract
      # those fields directly to avoid warning-prone full CSV reconstruction
      # from a lazy-data object that has already lost its original quoting.
      return(
        data.frame(
          study_id = sub("^([^,]*),.*$", "\\1", data_long_lines),
          participant_id = sub("^[^,]*,([^,]*).*$", "\\1", data_long_lines),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  ############################################################
  # 3) Fall back to the bundled CSV when available
  ############################################################

  # Resolve the data path through system.file() first so installed packages can
  # locate bundled files independent of the current working directory.
  data_long_path <- system.file(
    "data",
    "data_long.csv",
    package = "fearbase",
    mustWork = FALSE
  )

  # During local development the package may not be installed, so use the
  # repository-relative path if the installed-package lookup did not succeed.
  if (
    identical(data_long_path, "") &&
      file.exists(file.path("data", "data_long.csv"))
  ) {
    data_long_path <- file.path("data", "data_long.csv")
  }

  # Stop before readr sees an empty path, which would produce a less useful
  # file-system error than this direct package-level message.
  if (identical(data_long_path, "")) {
    stop(
      "`dl` must be supplied, an object named `data_long` must exist ",
      "in the calling environment, or bundled long-format data must be ",
      "available.",
      call. = FALSE
    )
  }

  # Read the convenience data lazily so package loading does not pay the cost
  # of parsing the full long-format data set unless the helper is called.
  readr::read_csv(data_long_path, show_col_types = FALSE)
}

#' Validate Sample-Size Grouping Input
#'
#' This internal helper centralizes validation for the two sample-size
#' functions so plotting and descriptive statistics reject the same unsupported
#' grouping declarations.
#'
#' @param dl A mapped long-format data frame.
#' @param grouping_variable A single grouping column name.
#'
#' @return Invisibly returns `grouping_variable` if validation succeeds.
#' @noRd
.validate_sample_size_grouping <- function(dl, grouping_variable) {
  ############################################################
  # 1) Validate the grouping declaration itself
  ############################################################

  # The downstream tidy-evaluation code expects exactly one string because the
  # plot and descriptives both aggregate by a single identifier column.
  if (!is.character(grouping_variable) || length(grouping_variable) != 1L) {
    stop("`grouping_variable` must be a single character string.", call. = FALSE)
  }

  # Restrict the public contract to study-level identifiers. Condition-level
  # identifiers would answer a different question than sample size by study.
  valid_group_vars <- c("study_id", "paper_study_id")

  # Reject unsupported grouping names before checking the data columns so typos
  # and conceptually unsupported identifiers receive a clear error.
  if (!grouping_variable %in% valid_group_vars) {
    stop(
      "`grouping_variable` must be one of: ",
      paste(valid_group_vars, collapse = ", "),
      call. = FALSE
    )
  }

  ############################################################
  # 2) Validate the mapped data schema required for aggregation
  ############################################################

  # The aggregation needs the requested study identifier and participant_id.
  # Report all missing columns at once to make input repair straightforward.
  required_cols <- c(grouping_variable, "participant_id")
  missing_cols <- setdiff(required_cols, names(dl))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(grouping_variable)
}

#' Prepare Distinct Participant Counts by Study
#'
#' This internal helper performs the shared data preparation for the
#' sample-size plot and descriptive-statistics functions.
#'
#' @param dl A data frame in long format, or `NULL`.
#' @param grouping_variable A single grouping column name.
#'
#' @return A tibble with one row per grouping value and a column `n` containing
#'   the number of distinct participants in that group.
#' @noRd
.prepare_sample_size_by_study <- function(
    dl = NULL,
    grouping_variable = "study_id"
) {
  ############################################################
  # 1) Resolve, validate, and normalize the long-format data
  ############################################################

  # Resolve omitted data before validation so the public helpers can be called
  # with either explicit data or the package's convenience data source.
  dl <- .resolve_sample_size_long_data(dl)

  # Tidyverse verbs and the mapping helper require a rectangular object with
  # named columns, so reject non-data-frame inputs before schema normalization.
  if (!is.data.frame(dl)) {
    stop("`dl` must be a data frame.", call. = FALSE)
  }

  # Apply the package-level mapping before checking grouping columns so callers
  # may provide current or legacy long-format FEARBASE schemas.
  dl <- .apply_mapping_to_long_data(dl)

  # Validate the requested grouping variable after mapping, because mapping can
  # create or normalize identifier columns used by the aggregation.
  .validate_sample_size_grouping(dl, grouping_variable)

  ############################################################
  # 2) Count distinct participants per requested study identifier
  ############################################################

  # Keep only the identifier columns needed for counting. distinct() prevents
  # long-format repeated measures from inflating participant counts.
  data_sample_size <- dl |>
    select(all_of(c(grouping_variable, "participant_id"))) |>
    filter(
      !is.na(.data[[grouping_variable]]),
      !is.na(.data$participant_id)
    ) |>
    distinct() |>
    group_by(.data[[grouping_variable]]) |>
    summarise(
      n = n(),
      .groups = "drop"
    ) |>
    arrange(desc(.data$n), .data[[grouping_variable]])

  # Convert the selected grouping column to a factor after aggregation. Using
  # base assignment avoids a dynamic `:=` expression that R CMD check reports as
  # an undefined global function in package code.
  data_sample_size[[grouping_variable]] <- as.factor(
    data_sample_size[[grouping_variable]]
  )

  # A plot or descriptive summary without any participant-count rows would hide
  # a data-quality problem, so fail with a direct message instead.
  if (nrow(data_sample_size) == 0L) {
    stop(
      "`dl` must contain at least one non-missing participant and grouping ",
      "identifier pair.",
      call. = FALSE
    )
  }

  return(data_sample_size)
}

#' Visualize Sample Sizes by Study
#'
#' Generates a horizontal bar plot showing the number of distinct participants
#' in each study-level group in a FEARBASE long-format data set.
#'
#' The input data are first passed through the package-internal long-format
#' mapping helper so current and legacy FEARBASE identifier schemas expose the
#' same study identifier columns before participant counts are computed.
#'
#' @param dl A data frame in long format. Must contain `participant_id` and the
#'   selected `grouping_variable` after `.apply_mapping_to_long_data()` is
#'   applied. If `NULL`, the function first attempts to use an object named
#'   `data_long` from the calling environment and then falls back to the
#'   package-bundled `data/data_long.csv` file.
#' @param grouping_variable A single character string specifying the study-level
#'   grouping column. Must be one of `"study_id"` or `"paper_study_id"`.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Resolves the long-format data source if `dl = NULL`.
#'   \item Validates that `dl` is a data frame.
#'   \item Applies `.apply_mapping_to_long_data()` to normalize identifier
#'     columns.
#'   \item Validates that the requested grouping column and `participant_id`
#'     are available after mapping.
#'   \item Removes rows with missing grouping identifiers or missing
#'     participant IDs.
#'   \item Counts distinct `participant_id` values within each group.
#'   \item Returns a `ggplot2` horizontal bar plot ordered by descending sample
#'     size.
#' }
#'
#' Distinct participant IDs are counted so repeated long-format rows for the
#' same participant do not inflate sample sizes. Groups with missing identifiers
#' and rows with missing participant IDs are excluded.
#'
#' @return A `ggplot2` object showing the number of distinct participants per
#'   study-level group.
#'
#' @examples
#' \dontrun{
#' sampleSizeByStudy(data_long)
#' sampleSizeByStudy(data_long, grouping_variable = "paper_study_id")
#' }
#'
#' @importFrom rlang .data
#' @export
sampleSizeByStudy <- function(
    dl = NULL,
    grouping_variable = "study_id"
) {
  ############################################################
  # 1) Prepare validated sample-size counts
  ############################################################

  # Delegate data resolution, mapping, validation, de-duplication, and counting
  # to the shared helper so this plotting function stays focused on graphics.
  data_sample_size <- .prepare_sample_size_by_study(
    dl = dl,
    grouping_variable = grouping_variable
  )

  ############################################################
  # 2) Build and return the sample-size plot
  ############################################################

  # Give the y-axis a small amount of headroom so the largest bar does not sit
  # flush against the panel boundary.
  y_upper_limit <- max(data_sample_size$n) + 10L

  # Build the plot from pre-aggregated participant counts. geom_col() is used
  # instead of geom_bar(stat = "identity") for clearer ggplot2 intent.
  graph <- data_sample_size |>
    ggplot(
      aes(
        x = forcats::fct_reorder(
          .data[[grouping_variable]],
          .x = .data$n,
          .desc = TRUE
        ),
        y = .data$n
      )
    ) +
    geom_col() +
    coord_flip(ylim = c(0, y_upper_limit)) +
    labs(
      x = grouping_variable,
      y = "Number of Participants"
    )

  # Return the ggplot object without printing so callers can add layers, themes,
  # or pass the plot to ggsave().
  return(graph)
}

#' Compute Descriptive Statistics for Sample Sizes by Study
#'
#' Computes descriptive statistics for the number of distinct participants in
#' each study-level group in a FEARBASE long-format data set.
#'
#' The input data are first passed through the package-internal long-format
#' mapping helper. Participant counts are then computed per requested
#' study-level grouping column and summarized with `psych::describe()`.
#'
#' @param dl A data frame in long format. Must contain `participant_id` and the
#'   selected `grouping_variable` after `.apply_mapping_to_long_data()` is
#'   applied. If `NULL`, the function first attempts to use an object named
#'   `data_long` from the calling environment and then falls back to the
#'   package-bundled `data/data_long.csv` file.
#' @param grouping_variable A single character string specifying the study-level
#'   grouping column. Must be one of `"study_id"` or `"paper_study_id"`.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Resolves the long-format data source if `dl = NULL`.
#'   \item Applies `.apply_mapping_to_long_data()` to normalize identifier
#'     columns.
#'   \item Counts distinct `participant_id` values within each requested group.
#'   \item Passes the resulting sample-size vector to `psych::describe()`.
#' }
#'
#' Distinct participant IDs are counted so repeated long-format rows for the
#' same participant do not inflate sample sizes. Groups with missing identifiers
#' and rows with missing participant IDs are excluded before descriptive
#' statistics are computed.
#'
#' @return A data frame returned by `psych::describe()` containing descriptive
#'   statistics for the per-study sample-size counts.
#'
#' @examples
#' \dontrun{
#' sampleSizeDescriptives(data_long)
#' sampleSizeDescriptives(data_long, grouping_variable = "paper_study_id")
#' }
sampleSizeDescriptives <- function(
    dl = NULL,
    grouping_variable = "study_id"
) {
  ############################################################
  # 1) Prepare validated sample-size counts
  ############################################################

  # Use the same preparation path as sampleSizeByStudy() so the plot and
  # descriptive statistics always summarize the identical participant counts.
  data_sample_size <- .prepare_sample_size_by_study(
    dl = dl,
    grouping_variable = grouping_variable
  )

  ############################################################
  # 2) Compute descriptive statistics for group-level sample sizes
  ############################################################

  # Delegate the descriptive-statistic calculations to psych::describe(),
  # matching the package's existing descriptive helper style.
  result <- psych::describe(data_sample_size$n)

  return(result)
}
