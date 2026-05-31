#' Resolve Long-Format Data for Sample-Size Helpers
#'
#' This internal helper keeps the public sample-size functions aligned on how
#' omitted data are resolved. It first uses explicit caller input, then looks
#' for a caller-side object named `data_long`.
#'
#' @param dl A data frame supplied by the caller, or `NULL`.
#'
#' @return A data frame in long format.
#' @noRd
.resolve_sample_size_long_data <- function(dl = NULL) {
  # Explicit data remain the reproducible primary path. The caller-side lookup
  # keeps interactive workflows convenient without silently reading ignored
  # real-data payloads from the local repository.
  .resolve_long_data(dl = dl, caller_env = parent.frame())
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
  # Restrict the public contract to study-level identifiers. Condition-level
  # identifiers would answer a different question than sample size by study.
  valid_group_vars <- c("study_id", "condition_id")
  grouping_variable <- .validate_choice(
    grouping_variable,
    "grouping_variable",
    valid_group_vars
  )

  ############################################################
  # 2) Validate the mapped data schema required for aggregation
  ############################################################

  # The aggregation needs the requested study identifier and participant_id.
  # Report all missing columns at once to make input repair straightforward.
  required_cols <- c(grouping_variable, "participant_id")
  .validate_required_columns(dl, required_cols, "dl")

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
  # with either explicit data or a caller-side data_long object.
  dl <- .resolve_sample_size_long_data(dl)

  # Tidyverse verbs and the mapping helper require a rectangular object with
  # named columns, so reject non-data-frame inputs before schema normalization.
  .validate_data_frame(dl, "dl")

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
#'   `data_long` from the calling environment.
#' @param grouping_variable A single character string specifying the study-level
#'   grouping column. Must be one of `"study_id"` or `"condition_id"`.
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
#' sample_size_by_study(data_long)
#' sample_size_by_study(data_long, grouping_variable = "condition_id")
#' }
#'
#' @importFrom rlang .data
#' @export
sample_size_by_study <- function(
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

  x_axis_title <- if (identical(grouping_variable, "study_id")) {
    "Study ID"
  } else {
    "Condition ID"
  }

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
      x = x_axis_title,
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
#'   `data_long` from the calling environment.
#' @param grouping_variable A single character string specifying the study-level
#'   grouping column. Must be one of `"study_id"` or `"condition_id"`.
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
#' sample_size_descriptives(data_long)
#' sample_size_descriptives(data_long, grouping_variable = "condition_id")
#' }
sample_size_descriptives <- function(
  dl = NULL,
  grouping_variable = "study_id"
) {
  ############################################################
  # 1) Prepare validated sample-size counts
  ############################################################

  # Use the same preparation path as sample_size_by_study() so the plot and
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
