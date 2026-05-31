#' Visualize Reinforcement Rates
#'
#' Creates a bar plot showing the distribution of reinforcement rates reported
#' in the FEARBASE study metadata.
#'
#' Metadata are first passed through the package-internal study-to-condition
#' mapping helper so current and legacy metadata schemas expose the same
#' identifier columns before reinforcement-rate columns are selected.
#'
#' @param md A data frame containing study metadata. Reinforcement-rate columns
#'   are identified after mapping by column names that start with `"reinf"`.
#'   If `NULL`, the function first attempts to use an object named `metadata`
#'   from the calling environment.
#' @param grouping_variable A single character string specifying whether
#'   reinforcement-rate values are counted by unique studies or unique
#'   conditions. Must be either `"study_id"` or `"condition_id"`.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Resolves the metadata source if `md = NULL`.
#'   \item Applies `.apply_mapping_to_metadata()` to normalize identifier
#'     columns.
#'   \item Validates that the requested grouping identifier and at least one
#'     reinforcement-rate column are available after mapping.
#'   \item Converts non-missing reinforcement-rate values to numeric values.
#'   \item Floors reinforcement rates to whole-number percentage points.
#'   \item Counts unique requested grouping identifiers per whole-number rate.
#'   \item Returns a `ggplot2` column chart.
#' }
#'
#' Non-missing reinforcement-rate values must be numeric or coercible to
#' numeric. Missing values are excluded from the plot. The function raises an
#' error if no valid reinforcement-rate values remain after validation.
#'
#' @return A `ggplot2` object showing the number of studies or conditions
#'   observed at each whole-number reinforcement rate.
#'
#' @examples
#' \dontrun{
#' reinforcement_rates(metadata)
#' reinforcement_rates(metadata, grouping_variable = "condition_id")
#' }
#'
#' @importFrom rlang .data
#' @export
reinforcement_rates <- function(md = NULL, grouping_variable = "study_id") {
  ############################################################
  # 1) Resolve the metadata source
  ############################################################

  # Resolve explicit data or a caller-side metadata object only. Ignored local
  # data files are intentionally not read by package code.
  md <- .resolve_metadata(md, caller_env = parent.frame())

  ############################################################
  # 2) Validate and normalize the metadata schema
  ############################################################

  # Every downstream operation assumes a rectangular object with named columns.
  # Failing here gives a clearer message than a later tidyverse method error.
  .validate_data_frame(md, "md")

  valid_grouping_variables <- c("study_id", "condition_id")
  grouping_variable <- .validate_choice(
    grouping_variable,
    "grouping_variable",
    valid_grouping_variables
  )

  # Apply the shared FEARBASE metadata mapping before checking for study_id and
  # reinforcement-rate columns so callers may supply current or legacy schemas.
  md <- .apply_mapping_to_metadata(md)

  # The requested identifier is retained during the wide-to-long transformation
  # and then used to count unique studies or conditions per reinforcement rate.
  if (!grouping_variable %in% names(md)) {
    stop(
      "`md` must contain a `",
      grouping_variable,
      "` column after mapping.",
      call. = FALSE
    )
  }

  # Reinforcement-rate metadata fields are currently stored in columns whose
  # names start with `reinf`. Resolve them explicitly so validation and
  # pivoting work from the same column set.
  reinforcement_columns <- names(md)[startsWith(names(md), "reinf")]

  # A plot cannot be produced if the mapped metadata contain no reinforcement
  # fields. Report this as an input-schema problem before reshaping.
  if (length(reinforcement_columns) == 0L) {
    stop(
      "`md` must contain at least one reinforcement-rate column whose name ",
      "starts with `reinf`.",
      call. = FALSE
    )
  }

  ############################################################
  # 3) Extract, coerce, and validate reinforcement-rate values
  ############################################################

  # Convert the selected wide reinforcement-rate columns into one value column
  # while retaining the requested identifier for de-duplicated aggregation.
  data_reinforcement_rate <- md |>
    select(
      all_of(c(grouping_variable, reinforcement_columns))
    ) |>
    tidyr::pivot_longer(
      cols = all_of(reinforcement_columns),
      names_to = "reinforcement_variable",
      values_to = "reinforcement_rate_raw"
    )

  # Stop on malformed values instead of silently dropping them, because a
  # non-numeric reinforcement rate indicates a data-quality or import problem.
  reinforcement_rate <- .coerce_numeric_strict(
    data_reinforcement_rate$reinforcement_rate_raw,
    "reinforcement-rate values"
  )

  # Store the validated numeric values on the plotting data frame so all later
  # operations use one normalized representation.
  data_reinforcement_rate$reinforcement_rate <- reinforcement_rate

  # Remove missing values before flooring and counting. Missing reinforcement
  # rates are absent metadata, not a reinforcement-rate category.
  data_reinforcement_rate <- data_reinforcement_rate |>
    filter(!is.na(.data$reinforcement_rate))

  # An empty chart would hide a data-quality problem, so stop with a direct
  # message if no valid values survived validation.
  if (nrow(data_reinforcement_rate) == 0L) {
    stop(
      "`md` must contain at least one non-missing reinforcement-rate value.",
      call. = FALSE
    )
  }

  ############################################################
  # 4) Count reinforcement-rate entries for plotting
  ############################################################

  # Floor reinforcement rates to whole-number percentage points to preserve the
  # original function's binning behavior while making the output deterministic.
  # De-duplicate by the selected identifier and binned rate before counting so
  # studies or conditions with the same rate recorded in multiple reinforcement
  # columns contribute once to that rate.
  data_reinforcement_rate <- data_reinforcement_rate |>
    mutate(
      reinforcement_rate = floor(.data$reinforcement_rate)
    ) |>
    distinct(
      .data[[grouping_variable]],
      .data$reinforcement_rate
    ) |>
    group_by(.data$reinforcement_rate) |>
    summarise(
      n = n(),
      .groups = "drop"
    ) |>
    arrange(.data$reinforcement_rate)

  # Use a 10-point tick interval over the observed range. The range is rounded
  # to multiples of ten so unusual values still receive readable axis breaks.
  x_breaks <- seq(
    from = floor(min(data_reinforcement_rate$reinforcement_rate) / 10) * 10,
    to = ceiling(max(data_reinforcement_rate$reinforcement_rate) / 10) * 10,
    by = 10
  )

  ############################################################
  # 5) Build and return the ggplot object
  ############################################################

  # Match the count axis title to the identifier used for de-duplication so the
  # plotted counts remain interpretable when callers switch aggregation levels.
  count_axis_title <- .count_axis_title(grouping_variable)

  # Build the plot from the aggregated counts and return the ggplot object
  # without printing so callers can add layers, themes, or save it.
  graph <- data_reinforcement_rate |>
    ggplot(
      aes(
        x = .data$reinforcement_rate,
        y = .data$n
      )
    ) +
    geom_col(color = "white") +
    geom_text(
      stat = "identity",
      aes(label = .data$reinforcement_rate),
      vjust = -1
    ) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
      x = "Reinforcement Rate",
      y = count_axis_title
    )

  return(graph)
}

#' Compute Descriptive Statistics for Reinforcement Rates
#'
#' Computes descriptive statistics for reinforcement rates reported in the
#' FEARBASE study metadata.
#'
#' Metadata are first passed through the package-internal study-to-condition
#' mapping helper. Reinforcement-rate columns are then selected by column names
#' that start with `"reinf"`, reshaped to a single numeric vector, and passed to
#' `psych::describe()`.
#'
#' @param md A data frame containing study metadata. Reinforcement-rate columns
#'   are identified after mapping by column names that start with `"reinf"`.
#'   If `NULL`, the function first attempts to use an object named `metadata`
#'   from the calling environment.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Resolves the metadata source if `md = NULL`.
#'   \item Applies `.apply_mapping_to_metadata()` to normalize identifier
#'     columns.
#'   \item Validates that `study_id` and at least one reinforcement-rate column
#'     are available after mapping.
#'   \item Converts non-missing reinforcement-rate values to numeric values.
#'   \item Removes missing reinforcement-rate values.
#'   \item Returns `psych::describe()` output for the cleaned numeric vector.
#' }
#'
#' Non-missing reinforcement-rate values must be numeric or coercible to
#' numeric. Missing values are excluded from the descriptive statistics.
#'
#' @return A data frame returned by `psych::describe()` containing descriptive
#'   statistics for all non-missing reinforcement-rate values.
#'
#' @examples
#' \dontrun{
#' reinforcement_rate_descriptives(metadata)
#' }
reinforcement_rate_descriptives <- function(md = NULL) {
  ############################################################
  # 1) Resolve the metadata source
  ############################################################

  # Mirror reinforcement_rates() so plots and descriptive summaries can be
  # called with the same metadata argument behavior.
  md <- .resolve_metadata(md, caller_env = parent.frame())

  ############################################################
  # 2) Validate and normalize the metadata schema
  ############################################################

  # The mapping helper and tidyverse reshaping below require data-frame input.
  .validate_data_frame(md, "md")

  # Normalize metadata identifiers before selecting reinforcement-rate columns.
  md <- .apply_mapping_to_metadata(md)

  # Keep the descriptives contract aligned with the plotting function by
  # requiring study_id after mapping.
  if (!"study_id" %in% names(md)) {
    stop("`md` must contain a `study_id` column after mapping.", call. = FALSE)
  }

  # Resolve all reinforcement-rate fields once so validation and pivoting use
  # exactly the same set of columns.
  reinforcement_columns <- names(md)[startsWith(names(md), "reinf")]

  # Without reinforcement-rate columns there is no well-defined numeric vector
  # to summarize.
  if (length(reinforcement_columns) == 0L) {
    stop(
      "`md` must contain at least one reinforcement-rate column whose name ",
      "starts with `reinf`.",
      call. = FALSE
    )
  }

  ############################################################
  # 3) Extract, coerce, and validate reinforcement-rate values
  ############################################################

  # Reshape wide reinforcement-rate metadata to a long table so every
  # reinforcement column contributes to one common descriptive vector.
  data_reinforcement_rate <- md |>
    select(
      all_of(c("study_id", reinforcement_columns))
    ) |>
    tidyr::pivot_longer(
      cols = all_of(reinforcement_columns),
      names_to = "reinforcement_variable",
      values_to = "reinforcement_rate_raw"
    )

  # Malformed non-missing values should fail clearly because they would make the
  # descriptive statistics depend on silent data loss.
  reinforcement_rate <- .coerce_numeric_strict(
    data_reinforcement_rate$reinforcement_rate_raw,
    "reinforcement-rate values"
  )

  # Keep only valid numeric values before calling psych::describe().
  reinforcement_rate <- reinforcement_rate[!is.na(reinforcement_rate)]

  # psych::describe() can technically handle empty input, but returning an
  # all-missing descriptive row would be less useful than a direct data message.
  if (length(reinforcement_rate) == 0L) {
    stop(
      "`md` must contain at least one non-missing reinforcement-rate value.",
      call. = FALSE
    )
  }

  ############################################################
  # 4) Compute descriptive statistics
  ############################################################

  # Delegate descriptive-statistic calculations to psych::describe(), matching
  # the package's existing descriptives helpers while keeping validation local.
  result <- psych::describe(reinforcement_rate)

  return(result)
}
