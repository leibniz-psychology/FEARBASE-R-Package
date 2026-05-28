#' Visualize Age Distributions from Long-Format Data
#'
#' Generates a visualization of participant age distributions from a dataset
#' in long format. The function supports either stacked histograms of age counts
#' or ridge (kernel density) plots grouped by a specified grouping variable.
#'
#' The input data must contain a column `measure` and a column `value`.
#' Only rows where `measure == "age"` are retained. The `value` column
#' is coerced to numeric to obtain age values. Rows with missing or
#' non-coercible age values are removed prior to plotting.
#'
#' @param dl A data frame in long format. Must contain at least the following columns:
#'   \itemize{
#'     \item `measure` (character): Variable identifier.
#'     \item `value` (numeric or character): Contains age values when `measure == "age"`.
#'     \item `participant_id` (numeric): Unique identifier of each participant.
#'     \item At least one of: `"condition_id"`, `"study_id"`,
#'           `"paper_cond_id"`, or `"paper_study_id"`.
#'   }
#'   The function `.apply_mapping_to_long_data()` is applied internally before
#'   further processing.
#'
#' @param type Character string specifying the plot type (case-insensitive).
#'   Must be one of:
#'   \itemize{
#'     \item `"histogram"`, `"hist"`, `"h"`: Stacked histogram of counts per age value.
#'     \item `"ridge"`, `"density"`, `"r"`, `"d"`: Kernel density ridge plot.
#'   }
#'
#' @param grouping_variable Character string specifying the grouping variable.
#'   Must be exactly one of:
#'   \itemize{
#'     \item `"condition_id"`
#'     \item `"study_id"`
#'     \item `"paper_cond_id"`
#'     \item `"paper_study_id"`
#'   }
#'   The selected variable must exist in `dl`. It is coerced to a factor.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Applies `.apply_mapping_to_long_data()` to `dl`.
#'   \item Filters rows where `measure == "age"`.
#'   \item Coerces `value` to numeric (`as.numeric()`).
#'   \item Removes rows with missing age values.
#'   \item Converts the grouping variable to a factor.
#'   \item Orders factor levels by descending mean age per group.
#' }
#'
#' For histogram plots, counts are computed per exact age value and group.
#' Bars are stacked and filled by the grouping variable.
#'
#' For ridge plots, kernel density estimates are computed using
#' `ggridges::geom_density_ridges()`.
#'
#' If `type` does not match any supported value, an error is raised.
#'
#' @return A `ggplot2` object.
#'
#' @section Dependencies:
#' This function requires:
#' \itemize{
#'   \item `dplyr`
#'   \item `ggplot2`
#'   \item `ggridges` (only for ridge/density plots)
#' }
#'
#' @seealso \code{\link[ggridges]{geom_density_ridges}}
#'
#' @examples
#' \dontrun{
#' # Histogram grouped by study
#' age(dl, type = "histogram", grouping_variable = "study_id")
#'
#' # Ridge density plot grouped by condition
#' age(dl, type = "ridge", grouping_variable = "condition_id")
#' }
#'
#' @importFrom rlang .data
#' @export
age <- function(
  dl,
  type = "histogram",
  grouping_variable = "study_id"
) {
  ############################################################
  # 1) Normalize the caller's long-format data schema
  ############################################################

  # Apply the package-level mapping before validation so callers can provide
  # either legacy identifiers or the current FEARBASE identifier columns.
  dl <- .apply_mapping_to_long_data(dl)

  ############################################################
  # 2) Validate plotting inputs and supported grouping choices
  ############################################################

  # All downstream filtering, selecting, and plotting assumes a rectangular
  # data object with named columns, so fail early for non-data-frame inputs.
  if (!is.data.frame(dl)) {
    stop("`dl` must be a data.frame.", call. = FALSE)
  }

  # These are the only grouping identifiers that this visualization knows how
  # to expose. Keep this vector close to validation because it is reused for
  # column selection and factor conversion below.
  valid_group_vars <- c(
    "condition_id",
    "study_id",
    "paper_cond_id",
    "paper_study_id"
  )

  # Reject unsupported grouping names before looking in the data. This gives a
  # clearer user-facing error when a typo or unsupported identifier is supplied.
  if (!grouping_variable %in% valid_group_vars) {
    stop(
      "`grouping_variable` must be one of: ",
      paste(valid_group_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # The requested grouping variable must survive the mapping step and be
  # present in the resulting data before it can be used by dplyr or ggplot2.
  if (!grouping_variable %in% names(dl)) {
    stop(
      "`grouping_variable` not found in `dl`.",
      call. = FALSE
    )
  }

  # Match plot types case-insensitively while preserving a small set of short
  # aliases for interactive use.
  type <- tolower(type)

  # Keep the aliases explicit so the branching condition below is easy to
  # audit when new plot types are added.
  valid_hist <- c("histogram", "hist", "h")
  valid_ridge <- c("ridge", "density", "r", "d")

  # Stop before any data transformation if the requested visualization mode is
  # not one of the supported histogram or ridge-density variants.
  if (!type %in% c(valid_hist, valid_ridge)) {
    stop(
      "`type` must be one of: histogram, hist, h, ridge, density, r, d.",
      call. = FALSE
    )
  }

  ############################################################
  # 3) Validate age-specific input columns and prepare plotting data
  ############################################################

  # The function needs measure/value to locate age rows and participant_id for
  # a stable participant-level plotting contract, even though participant_id is
  # not drawn directly in the final chart.
  required_cols <- c("measure", "value", "participant_id")
  missing_cols <- setdiff(required_cols, names(dl))

  # Report all missing required columns together so callers can fix the input
  # schema in one pass.
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Build a compact age-only data set for plotting. The value column is
  # intentionally coerced with suppressWarnings() because invalid values are
  # removed immediately after coercion and do not need one warning per row.
  data_age <- dl |>
    # Retain only long-format rows that actually encode participant age.
    filter(.data$measure == "age") |>
    # Keep all supported grouping columns that are present so the selected
    # grouping variable and any future grouping switches remain available.
    select(
      any_of(valid_group_vars),
      "participant_id",
      "value",
      "measure"
    ) |>
    # Store the plotting value in a dedicated numeric age column and normalize
    # grouping identifiers to factors for discrete legends and axes.
    mutate(
      age = suppressWarnings(as.numeric(.data$value)),
      across(
        any_of(valid_group_vars),
        as.factor
      )
    ) |>
    # Drop missing and non-coercible ages before ordering groups or estimating
    # density curves.
    filter(!is.na(.data$age))

  # A plot with no numeric age observations would be misleading and would fail
  # later in less obvious ways, so stop with a data-quality message here.
  if (nrow(data_age) == 0) {
    stop("No valid age data found after filtering.", call. = FALSE)
  }

  ############################################################
  # 4) Order groups by descending mean age for stable visual comparison
  ############################################################

  # Compute a single ordered level vector from the prepared age data. Ordering
  # by mean age keeps both histogram legends and ridge plot axes consistent
  # across repeated calls with the same input data.
  study_order <- data_age |>
    group_by(.data[[grouping_variable]]) |>
    summarise(
      mean_age = mean(.data$age, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(.data$mean_age) |>
    pull(all_of(grouping_variable))

  # Rebuild only the selected grouping factor with explicit levels. Other
  # grouping columns can remain as ordinary factors because they are not mapped
  # to the current plot.
  data_age[[grouping_variable]] <- factor(
    data_age[[grouping_variable]],
    levels = study_order
  )

  ############################################################
  # 5) Build the requested ggplot object
  ############################################################

  # Use the raw grouping column name as the legend or axis label. This keeps the
  # helper schema-oriented and avoids guessing at display labels.
  legend_label <- grouping_variable |>
    stringr::str_to_title() |>
    stringr::str_replace(pattern = "_id", replacement = " ID")

  if (type %in% valid_hist) {
    # Histograms are represented as exact age-by-group counts rather than
    # binned continuous histograms because age is expected to be reported in
    # interpretable units such as years.
    data_age <- data_age |>
      group_by(
        .data$age,
        .data[[grouping_variable]]
      ) |>
      summarise(
        n = n(),
        .groups = "drop"
      )

    # Draw one stacked bar per observed age value, with fill indicating the
    # requested grouping variable.
    graph <- ggplot(
      data_age,
      aes(x = .data$age, y = .data$n)
    ) +
      geom_bar(
        aes(fill = .data[[grouping_variable]]),
        stat = "identity",
        color = "white",
        linewidth = 0.2
      ) +
      labs(
        x = "Age",
        y = "Number of Participants",
        fill = legend_label
      ) +
      scale_fill_discrete(name = legend_label) +
      guides(fill = guide_legend(reverse = TRUE))
  } else {
    # Ridge plots use the unaggregated numeric ages so ggridges can estimate a
    # density curve separately for each ordered group.
    graph <- ggplot(
      data_age,
      aes(
        x = .data$age,
        y = .data[[grouping_variable]],
        group = .data[[grouping_variable]],
        fill = .data[[grouping_variable]]
      )
    ) +
      ggridges::geom_density_ridges() +
      labs(
        x = "Age",
        y = legend_label,
        fill = legend_label
      ) +
      # # Keep the ridge fills on the package palette while reversing the color
      # # assignment so the ordered y-axis reads from the opposite palette end.
      # scale_fill_discrete(
      #   palette = function(n) {
      #     rev(generate_palette(n))
      #   }
      # ) +
      theme(
        # The group is already shown on the y-axis, so suppress the duplicate
        # fill legend for density/ridge output.
        legend.position = "none"
      )
  }

  # Return the plot without printing so callers can add layers, theme it, or
  # pass it to ggsave().
  return(graph)
}

#' Compute Descriptive Statistics for Age
#'
#' Computes descriptive statistics for participant age from a dataset in
#' long format. The function extracts rows where `measure == "age"`,
#' coerces the `value` column to numeric, removes missing age values,
#' and returns summary statistics optionally grouped by one or more variables.
#'
#' @param dl A data frame in long format. Must contain at least the following columns:
#'   \itemize{
#'     \item `measure` (character): Variable identifier.
#'     \item `value` (numeric or character): Contains age values when `measure == "age"`.
#'   }
#'   The function `.apply_mapping_to_long_data()` is applied internally before
#'   further processing.
#'
#' @param grouping_variable Optional character vector specifying one or more
#'   column names in `dl` used for grouping. All specified variables must exist
#'   in `dl`. If `NULL` (default), descriptive statistics are computed across
#'   the entire dataset without grouping.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Applies `.apply_mapping_to_long_data()` to `dl`.
#'   \item Filters rows where `measure == "age"`.
#'   \item Coerces `value` to numeric using `as.numeric()`.
#'   \item Removes rows with missing age values.
#'   \item Optionally groups by `grouping_variable`.
#'   \item Computes descriptive statistics.
#' }
#'
#' The following statistics are returned:
#' \itemize{
#'   \item `mean_age`: Arithmetic mean of age.
#'   \item `sd`: Sample standard deviation of age.
#'   \item `min`: Minimum observed age.
#'   \item `max`: Maximum observed age.
#' }
#'
#' The standard deviation is computed using \code{\link[stats]{sd}},
#' corresponding to the sample standard deviation with denominator n−1 n - 1 n−1.
#'
#' Rows with non-coercible or missing age values are removed prior to
#' computation.
#'
#' @return
#' A tibble (or data frame) with one row per group (or one row total if
#' `grouping_variable = NULL`) containing:
#' \itemize{
#'   \item Grouping columns (if specified)
#'   \item `mean_age`
#'   \item `sd`
#'   \item `min`
#'   \item `max`
#' }
#'
#' @section Dependencies:
#' This function requires:
#' \itemize{
#'   \item `dplyr`
#' }
#'
#' @examples
#' \dontrun{
#' # Overall descriptives
#' ageDescriptives(dl)
#'
#' # Descriptives grouped by study
#' ageDescriptives(dl, grouping_variable = "study_id")
#'
#' # Descriptives grouped by multiple variables
#' ageDescriptives(dl, grouping_variable = c("study_id", "condition_id"))
#' }
#'
#' @export
ageDescriptives <- function(dl, grouping_variable = NULL) {
  ############################################################
  # 1) Normalize the caller's long-format data schema
  ############################################################

  # Use the same mapping path as age() so descriptive summaries and plots agree
  # on the identifier columns available after schema normalization.
  dl <- .apply_mapping_to_long_data(dl)

  ############################################################
  # 2) Validate descriptive-statistics inputs
  ############################################################

  # dplyr verbs below require a data frame or tibble with named columns.
  if (!is.data.frame(dl)) {
    stop("`dl` must be a data.frame.", call. = FALSE)
  }

  # Descriptives only require the long-format measure/value pair. Grouping
  # columns are validated separately because they are optional.
  required_cols <- c("measure", "value")
  missing_cols <- setdiff(required_cols, names(dl))

  # Return every missing required column in one message to make malformed input
  # easier to repair.
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # If grouping is requested, verify both the type of the grouping declaration
  # and the existence of every named grouping column before any summarisation.
  if (!is.null(grouping_variable)) {
    # Multiple grouping variables are allowed, but they must be supplied as
    # column names in a character vector.
    if (!is.character(grouping_variable)) {
      stop("`grouping_variable` must be a character vector.", call. = FALSE)
    }

    # Check all requested grouping columns against the mapped data schema.
    missing_group_cols <- setdiff(grouping_variable, names(dl))

    # Report all missing grouping columns together so callers can correct a
    # multi-column grouping request without repeated trial and error.
    if (length(missing_group_cols) > 0) {
      stop(
        "Grouping variable(s) not found in `dl`: ",
        paste(missing_group_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  ############################################################
  # 3) Extract and clean numeric age observations
  ############################################################

  # Keep age rows from the long-format data and coerce their values to numeric
  # for statistical summaries. Invalid numeric conversions are filtered out
  # after coercion.
  df <- dl |>
    filter(.data$measure == "age") |>
    mutate(
      age = suppressWarnings(as.numeric(.data$value))
    ) |>
    filter(!is.na(.data$age))

  # A descriptive table with no valid observations would produce undefined
  # summary values, so stop before returning all-NA statistics.
  if (nrow(df) == 0) {
    stop("No valid age data found after filtering.", call. = FALSE)
  }

  ############################################################
  # 4) Apply optional grouping for grouped summaries
  ############################################################

  if (!is.null(grouping_variable)) {
    # Convert grouping columns to factors so returned grouping keys use a stable
    # discrete representation, matching the plotting helper's behavior.
    df <- df |>
      mutate(
        across(
          all_of(grouping_variable),
          as.factor
        )
      ) |>
      # group_by(across()) supports one or many caller-supplied grouping
      # variables without branching on the length of grouping_variable.
      group_by(
        across(all_of(grouping_variable))
      )
  }

  ############################################################
  # 5) Compute age summary statistics
  ############################################################

  # Summarise the cleaned age values either globally or within the grouping
  # structure created above. .groups = "drop" returns an ungrouped tibble that
  # is easier for callers to print, join, or export.
  result <- df |>
    summarise(
      mean_age = mean(.data$age, na.rm = TRUE),
      sd_age = stats::sd(.data$age, na.rm = TRUE),
      min_age = min(.data$age, na.rm = TRUE),
      max_age = max(.data$age, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  # Return the summary table to the caller without additional formatting.
  return(result)
}

# Todo:
# - plot titles
# - (dynamic) axis labels
# - Fix Legend Titles
