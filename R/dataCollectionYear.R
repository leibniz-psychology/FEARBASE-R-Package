#' Plot the Number of Studies by Data Collection Year
#'
#' @description
#' Creates a bar plot showing how many studies in the metadata were collected
#' in each calendar year. Metadata are first passed through the package-internal
#' study-to-condition mapping so that legacy and current metadata schemas are
#' handled consistently.
#'
#' @param md A data frame containing study metadata. The data frame must contain
#'   a `year` column. If `NULL`, the function first attempts to use an object
#'   named `metadata` from the calling environment and then falls back to the
#'   package-bundled `data/metadata.csv` file.
#' @param grouping_variable A single character string specifying whether data
#'   collection years are counted by unique studies or unique conditions. Must
#'   be either `"study_id"` or `"condition_id"`.
#' @param year_of A single character string specifying which year variable is
#'   plotted. Must be either `"publication"` to use `md$year` or `"data"` to
#'   use `md$year_data`.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Applies `.apply_mapping_to_metadata()` to `md`.
#'   \item Validates that the requested year column and grouping identifier are
#'     present.
#'   \item Coerces non-missing selected year values to numeric values.
#'   \item Counts unique requested grouping identifiers per year after removing
#'     missing year values.
#'   \item Returns a `ggplot2` column chart.
#' }
#'
#' Non-missing values in the selected year column must be numeric or coercible
#' to numeric. Missing values are excluded from the plot. The function raises an
#' error if no valid year values remain after validation.
#'
#' @return A `ggplot2` object showing one column per data collection year.
#'
#' @examples
#' \dontrun{
#' dataCollectionYear(metadata)
#' dataCollectionYear(metadata, grouping_variable = "condition_id")
#' dataCollectionYear(metadata, year_of = "data")
#' }
#'
#' @importFrom rlang .data
#' @export
dataCollectionYear <- function(
  md = NULL,
  grouping_variable = "study_id",
  year_of = "publication"
) {
  ############################################################
  # 1) Resolve the metadata source
  ############################################################

  # Allow callers to omit `md` for interactive package use. In that case, look
  # first for a `metadata` object in the caller's environment and only then fall
  # back to the package-bundled metadata file.
  if (is.null(md)) {
    # Prefer the caller's in-memory metadata object when it exists because it is
    # likely to reflect the data currently being analyzed or edited.
    if (exists("metadata", envir = parent.frame(), inherits = TRUE)) {
      md <- get("metadata", envir = parent.frame(), inherits = TRUE)
    } else {
      # Record the installed package metadata path. `mustWork = FALSE` lets us
      # detect a missing bundled file and produce a package-specific error below.
      metadata_path <- system.file(
        "data",
        "metadata.csv",
        package = "fearbase",
        mustWork = FALSE
      )
    }

    # If no caller-supplied object was found and the package data file is not
    # available, stop before attempting to read from an empty path.
    if (is.null(md) && identical(metadata_path, "")) {
      stop(
        "`md` must be supplied, an object named `metadata` must exist ",
        "in the calling environment, or bundled metadata must be available.",
        call. = FALSE
      )
    }

    if (is.null(md)) {
      # Use the package-bundled metadata only as a final fallback so explicit
      # user input remains the primary and reproducible data source.
      md <- readr::read_csv(metadata_path, show_col_types = FALSE)
    }
  }

  ############################################################
  # 2) Validate and normalize the metadata schema
  ############################################################

  # All subsequent column checks, assignments, and plotting preparation require
  # a rectangular data object with named columns.
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

  # The grouping identifier determines the unit counted for each collection
  # year. Require one explicit supported value so aggregation and axis labeling
  # remain predictable.
  if (
    !is.character(grouping_variable) ||
      length(grouping_variable) != 1L ||
      is.na(grouping_variable)
  ) {
    stop(
      "`grouping_variable` must be a single non-missing character string.",
      call. = FALSE
    )
  }

  valid_grouping_variables <- c("study_id", "condition_id")

  # Match the public API used by the other metadata summary plots: study-level
  # counts collapse mapped conditions into their parent study, whereas
  # condition-level counts keep each condition as its own observation.
  if (!grouping_variable %in% valid_grouping_variables) {
    stop(
      "`grouping_variable` must be one of: ",
      paste(valid_grouping_variables, collapse = ", "),
      call. = FALSE
    )
  }

  # The year switch selects between publication-year metadata and data-upload
  # year metadata while keeping the rest of the counting pipeline identical.
  if (
    !is.character(year_of) ||
      length(year_of) != 1L ||
      is.na(year_of)
  ) {
    stop(
      "`year_of` must be a single non-missing character string.",
      call. = FALSE
    )
  }

  valid_year_of <- c("publication", "data")

  # Restrict the selector to the two metadata columns with clearly defined plot
  # semantics. This prevents accidental counting of unrelated year-like fields.
  if (!year_of %in% valid_year_of) {
    stop(
      "`year_of` must be one of: ",
      paste(valid_year_of, collapse = ", "),
      call. = FALSE
    )
  }

  year_column <- if (identical(year_of, "publication")) {
    "year"
  } else {
    "year_data"
  }

  # Apply the package's metadata mapping before validating selected columns so
  # legacy and current metadata inputs expose the same names downstream.
  md <- .apply_mapping_to_metadata(md)

  # The plot is organized around the requested year field, so fail early if the
  # mapped metadata do not contain the selected source column.
  if (!year_column %in% names(md)) {
    stop(
      "`md` must contain a `",
      year_column,
      "` column.",
      call. = FALSE
    )
  }

  # The requested identifier must exist after mapping because it is used to
  # de-duplicate observations before counting each collection year.
  if (!grouping_variable %in% names(md)) {
    stop(
      "`md` must contain a `",
      grouping_variable,
      "` column after mapping.",
      call. = FALSE
    )
  }

  ############################################################
  # 3) Coerce and validate year values
  ############################################################

  # Coerce years explicitly so the downstream scale receives numeric values and
  # malformed, non-missing years fail with a clear input-validation error.
  raw_year <- md[[year_column]]

  # Preserve numeric vectors as-is apart from normalizing their storage mode;
  # otherwise coerce through character so factors and labelled values convert by
  # their displayed values rather than by underlying integer codes.
  if (is.numeric(raw_year)) {
    year <- as.numeric(raw_year)
  } else {
    year <- suppressWarnings(as.numeric(as.character(raw_year)))
  }

  # A value is invalid only when the caller supplied something non-missing that
  # could not be represented as a number. Missing values are handled separately
  # and are intentionally excluded from the final plot.
  invalid_year <- !is.na(raw_year) & is.na(year)

  # Stop on malformed non-missing years instead of silently dropping them; a
  # typo in a year should be treated as a data-quality issue.
  if (any(invalid_year)) {
    stop(
      "All non-missing values in `md$",
      year_column,
      "` must be numeric or coercible ",
      "to numeric.",
      call. = FALSE
    )
  }

  # Store the validated numeric vector in a standardized internal column so the
  # counting and plotting code can stay identical for publication and data
  # upload years. The final plot data still exposes this as `year`.
  md$selected_year <- year

  # Remove missing years for counting and plotting. Missing data are excluded
  # from the visualization rather than represented as a pseudo-year category.
  valid_year <- md$selected_year[!is.na(md$selected_year)]

  # A chart with no valid years would be empty and misleading, so stop with a
  # direct input-quality message.
  if (length(valid_year) == 0L) {
    stop(
      "`md$",
      year_column,
      "` must contain at least one non-missing value.",
      call. = FALSE
    )
  }

  ############################################################
  # 4) Count studies per year and prepare axis breaks
  ############################################################

  # Count each study or condition once per year. This prevents repeated rows for
  # the same identifier from inflating frequencies while still allowing truly
  # distinct mapped conditions to be counted when requested.
  data_collection_year <- md |>
    filter(!is.na(.data$selected_year)) |>
    distinct(
      .data[[grouping_variable]],
      .data$selected_year
    ) |>
    count(.data$selected_year, name = "n") |>
    rename(year = all_of("selected_year")) |>
    arrange(.data$year)

  # Build one tick mark per calendar year in the observed range, including
  # years with zero studies so gaps in the timeline remain visually apparent.
  year_breaks <- seq(
    from = floor(min(data_collection_year$year)),
    to = ceiling(max(data_collection_year$year)),
    by = 1
  )

  ############################################################
  # 5) Build and return the ggplot object
  ############################################################

  # Match the count axis title to the identifier used for de-duplication so the
  # graph remains self-explanatory when callers switch aggregation levels.
  count_axis_title <- if (identical(grouping_variable, "study_id")) {
    "Number of Studies"
  } else {
    "Number of Conditions"
  }

  # Keep the publication-year label unchanged for backward compatibility. The
  # data-year label follows the requested user-facing wording.
  year_axis_title <- if (identical(year_of, "publication")) {
    "Year of Publication"
  } else {
    "Year of Data Uplod"
  }

  # Use the .data pronoun so the data frame can retain ordinary column names
  # while satisfying R CMD check for package code.
  graph <- data_collection_year |>
    ggplot(
      aes(
        x = .data$year,
        y = .data$n
      )
    ) +
    geom_col(fill = "#0032A0") +
    labs(x = year_axis_title, y = count_axis_title) +
    scale_x_continuous(breaks = year_breaks) +
    scale_y_continuous(
      breaks = seq(0L, ceiling(max(data_collection_year$n) / 2) * 2, by = 2),
      limits = c(0L, ceiling(max(data_collection_year$n) / 2) * 2)
    )

  # Return the plot object without printing it so callers can add layers,
  # change themes, or save it with ggsave().
  return(graph)
}
