#' Normalize Sex Values for Participant-Level Counts
#'
#' This internal helper converts common long-format sex or gender values to the
#' three categories used by the public `sex()` visualization.
#'
#' @param value A vector containing raw sex or gender values.
#'
#' @return A character vector with values `"m"`, `"f"`, or `"not reported"`.
#' @noRd
.normalize_sex_value <- function(value) {
  ############################################################
  # 1) Convert incoming values to a normalized character vector
  ############################################################

  # Coerce through character so factors, labelled values, and numeric imports
  # are interpreted by their displayed values instead of their internal codes.
  value <- stringr::str_squish(tolower(as.character(value)))

  ############################################################
  # 2) Collapse supported values to the plotting categories
  ############################################################

  # Match complete words where possible and keep first-letter support for the
  # existing FEARBASE coding style ("m", "f", "n"). Missing, empty, unknown,
  # and explicitly non-reported values are counted as "not reported".
  dplyr::case_when(
    is.na(value) | value == "" ~ "not reported",
    value %in%
      c(
        "n",
        "na",
        "n/a",
        "nr",
        "not reported",
        "not_reported",
        "not specified",
        "unknown"
      ) ~ "not reported",
    startsWith(value, "m") ~ "m",
    startsWith(value, "f") ~ "f",
    TRUE ~ NA_character_
  )
}

#' Visualize the Participant Sex Distribution
#'
#' Generates a pie chart showing the participant-level distribution of reported
#' sex or gender values in a FEARBASE long-format data set.
#'
#' The input data are first passed through the package-internal long-format
#' mapping helper so current and legacy FEARBASE identifier schemas expose the
#' same `study_id` and `condition_id` columns before participant-level counts
#' are computed.
#'
#' @param dl A data frame in long format. Must contain `study_id`,
#'   `participant_id`, `measure`, and `value` after
#'   `.apply_mapping_to_long_data()` is applied. If `NULL`, the function first
#'   attempts to use an object named `data_long` from the calling environment
#'   and then falls back to the package-bundled `data/data_long.csv` file.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Resolves the long-format data source if `dl = NULL`.
#'   \item Validates that `dl` is a data frame.
#'   \item Applies `.apply_mapping_to_long_data()` to normalize identifier
#'     columns.
#'   \item Validates that the required long-format columns are available after
#'     mapping.
#'   \item Extracts rows where `measure` is either `"sex"` or `"gender"`.
#'   \item Normalizes common sex or gender values to `"m"`, `"f"`, or
#'     `"not reported"`.
#'   \item Adds one `"not reported"` row for each participant without any sex
#'     or gender record.
#'   \item Counts one category per distinct `study_id` and `participant_id`
#'     pair and returns a `ggplot2` pie chart.
#' }
#'
#' Participant counts are computed after de-duplicating long-format rows so
#' repeated measures do not inflate the distribution. If a participant has
#' contradictory non-missing sex or gender categories, the function raises an
#' error instead of choosing one category silently.
#'
#' @return A `ggplot2` object showing participant counts by sex category.
#'
#' @examples
#' \dontrun{
#' sex(data_long)
#' }
#'
#' @importFrom rlang .data
#' @export
sex <- function(dl = NULL) {
  ############################################################
  # 1) Resolve and validate the long-format data source
  ############################################################

  # Reuse the package's existing long-data resolver so zero-argument calls keep
  # working for interactive users, tests, and installed-package examples.
  dl <- .resolve_sample_size_long_data(dl)

  # Tidyverse verbs and the mapping helper require a rectangular object with
  # named columns, so fail before any schema normalization for invalid inputs.
  if (!is.data.frame(dl)) {
    stop("`dl` must be a data frame.", call. = FALSE)
  }

  # Apply the shared FEARBASE mapping before validating required columns so
  # callers may supply current or legacy long-format identifier schemas.
  dl <- .apply_mapping_to_long_data(dl)

  # The pie chart is participant-level: study_id and participant_id define the
  # participant key, while measure/value identify sex or gender observations.
  required_cols <- c("study_id", "participant_id", "measure", "value")
  missing_cols <- setdiff(required_cols, names(dl))

  # Report every missing column at once so malformed inputs are easy to repair.
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ############################################################
  # 2) Build the distinct participant index
  ############################################################

  # Keep exactly one row per study-participant pair. Rows with missing
  # participant IDs cannot be counted as distinct participants, so they are
  # excluded from the participant-level denominator.
  participant_data <- dl |>
    select(
      all_of(c("study_id", "participant_id"))
    ) |>
    filter(!is.na(.data$participant_id)) |>
    distinct()

  # A pie chart without any identifiable participants would be misleading, so
  # stop with a direct data-quality message before deriving categories.
  if (nrow(participant_data) == 0L) {
    stop(
      "`dl` must contain at least one non-missing participant ID.",
      call. = FALSE
    )
  }

  ############################################################
  # 3) Extract and normalize reported sex or gender rows
  ############################################################

  # Select only the long-format rows that encode participant sex or gender and
  # normalize their values to the categories used in the plot.
  reported_sex <- dl |>
    filter(.data$measure %in% c("sex", "gender")) |>
    select(
      all_of(c("study_id", "participant_id", "value"))
    ) |>
    filter(!is.na(.data$participant_id)) |>
    mutate(
      sex = .normalize_sex_value(.data$value)
    )

  # Unsupported non-missing values are treated as data errors. This keeps the
  # distribution from silently dropping participants with unexpected coding.
  invalid_sex <- !is.na(reported_sex$value) & is.na(reported_sex$sex)

  if (any(invalid_sex)) {
    stop(
      "All non-missing sex or gender values must start with `m`, `f`, ",
      "or encode a missing/not-reported value.",
      call. = FALSE
    )
  }

  # Remove the raw value column after validation so duplicate checks operate on
  # participant-category combinations rather than repeated long-format records.
  reported_sex <- reported_sex |>
    select(
      all_of(c("study_id", "participant_id", "sex"))
    ) |>
    distinct()

  ############################################################
  # 4) Validate one category per participant
  ############################################################

  # A participant may have repeated identical sex/gender rows, but conflicting
  # categories would make the participant-level count ambiguous.
  conflicting_participants <- reported_sex |>
    group_by(
      .data$study_id,
      .data$participant_id
    ) |>
    summarise(
      n_categories = n_distinct(.data$sex),
      .groups = "drop"
    ) |>
    filter(.data$n_categories > 1L)

  # Stop instead of choosing the first category, because contradictory sex or
  # gender records indicate an input data-quality problem.
  if (nrow(conflicting_participants) > 0L) {
    stop(
      "`dl` contains contradictory sex or gender values for at least one ",
      "participant.",
      call. = FALSE
    )
  }

  ############################################################
  # 5) Add explicit not-reported rows for participants without reports
  ############################################################

  # Identify participants that have no sex/gender record and add them to the
  # distribution as "not reported" so the denominator remains all participants.
  unreported_sex <- participant_data |>
    anti_join(
      reported_sex,
      by = c("study_id", "participant_id")
    ) |>
    mutate(
      sex = "not reported"
    )

  # Combine reported and inferred not-reported participant rows before
  # aggregation. The result still contains one row per study-participant pair.
  data_sex <- bind_rows(
    reported_sex,
    unreported_sex
  )

  ############################################################
  # 6) Count sex categories for plotting
  ############################################################

  # Convert to an ordered factor so the pie chart labels and colors are stable
  # across calls, even when one category is absent from a particular dataset.
  data_sex <- data_sex |>
    mutate(
      sex = factor(
        .data$sex,
        levels = c("m", "f", "not reported")
      )
    )

  # Count participants in each category. .drop = FALSE preserves zero-count
  # categories for stable data shape; labels are filtered to positive counts.
  count_data <- data_sex |>
    group_by(.data$sex, .drop = FALSE) |>
    summarise(
      n = n(),
      .groups = "drop"
    )

  # Labels should appear only for categories that are present in the data.
  label_data <- count_data |>
    filter(.data$n > 0L)

  ############################################################
  # 7) Build and return the ggplot object
  ############################################################

  # Build a one-column stacked bar and project it into polar coordinates, which
  # is ggplot2's standard pie-chart construction.
  graph <- ggplot(
    count_data,
    aes(
      x = "",
      y = .data$n,
      fill = .data$sex
    )
  ) +
    geom_col(width = 1) +
    coord_polar("y", start = 0) +
    labs(fill = "Sex") +
    theme_void() +
    geom_label(
      data = label_data,
      aes(
        label = paste0(.data$sex, " (", .data$n, ")"),
        group = .data$sex
      ),
      position = position_stack(vjust = 0.5),
      fill = "white"
    ) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "transparent", color = NA)
    )

  # Return the plot without printing so callers can add layers, theme it, or
  # pass it to ggsave().
  return(graph)
}
