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
#'   from the calling environment and then falls back to the package-bundled
#'   `data/metadata.csv` file.
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
#'   \item Floors reinforcement rates to whole-number percentage points.
#'   \item Counts non-missing reinforcement-rate entries per whole-number rate.
#'   \item Returns a `ggplot2` column chart.
#' }
#'
#' Non-missing reinforcement-rate values must be numeric or coercible to
#' numeric. Missing values are excluded from the plot. The function raises an
#' error if no valid reinforcement-rate values remain after validation.
#'
#' @return A `ggplot2` object showing the number of reinforcement-rate entries
#'   observed at each whole-number reinforcement rate.
#'
#' @examples
#' \dontrun{
#' reinforcementRates(metadata)
#' }
#'
#' @importFrom rlang .data
#' @export
reinforcementRates <- function(md = NULL) {
  ############################################################
  # 1) Resolve the metadata source
  ############################################################

  # Let interactive users omit `md` while keeping explicit function arguments
  # as the preferred, reproducible data source.
  if (is.null(md)) {
    # Use a caller-side `metadata` object first because it likely reflects the
    # data currently being explored in the user's analysis session.
    if (exists("metadata", envir = parent.frame(), inherits = TRUE)) {
      md <- get("metadata", envir = parent.frame(), inherits = TRUE)
    } else {
      # Resolve the installed package data path lazily so a missing bundled
      # file can be reported with a package-specific error below.
      metadata_path <- system.file(
        "data",
        "metadata.csv",
        package = "fearbase",
        mustWork = FALSE
      )
    }

    # Stop before readr sees an empty path, which would otherwise create a less
    # helpful file-system error for users.
    if (is.null(md) && identical(metadata_path, "")) {
      stop(
        "`md` must be supplied, an object named `metadata` must exist ",
        "in the calling environment, or bundled metadata must be available.",
        call. = FALSE
      )
    }

    if (is.null(md)) {
      # Use package-bundled metadata only as a final fallback for examples,
      # tests, and interactive calls where no explicit data were supplied.
      md <- readr::read_csv(metadata_path, show_col_types = FALSE)
    }
  }

  ############################################################
  # 2) Validate and normalize the metadata schema
  ############################################################

  # Every downstream operation assumes a rectangular object with named columns.
  # Failing here gives a clearer message than a later tidyverse method error.
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

  # Apply the shared FEARBASE metadata mapping before checking for study_id and
  # reinforcement-rate columns so callers may supply current or legacy schemas.
  md <- .apply_mapping_to_metadata(md)

  # The plot uses study_id as the stable identifier retained during the
  # wide-to-long transformation, so it must be present after mapping.
  if (!"study_id" %in% names(md)) {
    stop("`md` must contain a `study_id` column after mapping.", call. = FALSE)
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
  # while retaining study_id for traceability and possible downstream auditing.
  data_reinforcement_rate <- md |>
    select(
      all_of(c("study_id", reinforcement_columns))
    ) |>
    tidyr::pivot_longer(
      cols = all_of(reinforcement_columns),
      names_to = "reinforcement_variable",
      values_to = "reinforcement_rate_raw"
    )

  # Coerce through character for non-numeric vectors so factors and labelled
  # values are interpreted by their displayed values rather than integer codes.
  reinforcement_rate <- suppressWarnings(as.numeric(as.character(
    data_reinforcement_rate$reinforcement_rate_raw
  )))

  # Treat only supplied, non-missing values that fail numeric coercion as
  # invalid. Missing values are expected in sparse metadata and are dropped.
  invalid_reinforcement_rate <- !is.na(
    data_reinforcement_rate$reinforcement_rate_raw
  ) & is.na(reinforcement_rate)

  # Stop on malformed values instead of silently dropping them, because a
  # non-numeric reinforcement rate indicates a data-quality or import problem.
  if (any(invalid_reinforcement_rate)) {
    stop(
      "All non-missing reinforcement-rate values must be numeric or ",
      "coercible to numeric.",
      call. = FALSE
    )
  }

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
  data_reinforcement_rate <- data_reinforcement_rate |>
    mutate(
      reinforcement_rate = floor(.data$reinforcement_rate)
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
    scale_x_continuous(breaks = x_breaks) +
    labs(
      x = "Reinforcement Rate",
      y = "Number of Reinforcement-Rate Entries"
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
#'   from the calling environment and then falls back to the package-bundled
#'   `data/metadata.csv` file.
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
#' reinforcementRateDescriptives(metadata)
#' }
reinforcementRateDescriptives <- function(md = NULL) {
  ############################################################
  # 1) Resolve the metadata source
  ############################################################

  # Mirror reinforcementRates() so plots and descriptive summaries can be
  # called with the same metadata argument behavior.
  if (is.null(md)) {
    # Prefer caller-side data because it is likely to be the dataset currently
    # under analysis and may differ from the package-bundled metadata.
    if (exists("metadata", envir = parent.frame(), inherits = TRUE)) {
      md <- get("metadata", envir = parent.frame(), inherits = TRUE)
    } else {
      # Resolve the package data file only when no in-memory metadata object was
      # supplied or found.
      metadata_path <- system.file(
        "data",
        "metadata.csv",
        package = "fearbase",
        mustWork = FALSE
      )
    }

    # Give a direct user-facing error if no explicit, in-memory, or bundled
    # metadata source is available.
    if (is.null(md) && identical(metadata_path, "")) {
      stop(
        "`md` must be supplied, an object named `metadata` must exist ",
        "in the calling environment, or bundled metadata must be available.",
        call. = FALSE
      )
    }

    if (is.null(md)) {
      # Read bundled metadata as a convenience fallback for tests, examples, and
      # interactive use.
      md <- readr::read_csv(metadata_path, show_col_types = FALSE)
    }
  }

  ############################################################
  # 2) Validate and normalize the metadata schema
  ############################################################

  # The mapping helper and tidyverse reshaping below require data-frame input.
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

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

  # Convert through character to avoid factor integer-code coercion and to make
  # the numeric validation behavior consistent across input column classes.
  reinforcement_rate <- suppressWarnings(as.numeric(as.character(
    data_reinforcement_rate$reinforcement_rate_raw
  )))

  # Flag only non-missing source values that could not be represented as
  # numbers. Missing values are dropped below and are not invalid by themselves.
  invalid_reinforcement_rate <- !is.na(
    data_reinforcement_rate$reinforcement_rate_raw
  ) & is.na(reinforcement_rate)

  # Malformed non-missing values should fail clearly because they would make the
  # descriptive statistics depend on silent data loss.
  if (any(invalid_reinforcement_rate)) {
    stop(
      "All non-missing reinforcement-rate values must be numeric or ",
      "coercible to numeric.",
      call. = FALSE
    )
  }

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
