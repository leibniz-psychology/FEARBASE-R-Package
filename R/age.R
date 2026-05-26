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
#' @importFrom dplyr filter select mutate across group_by summarise arrange pull any_of
#' @importFrom ggplot2 ggplot aes geom_bar labs scale_fill_discrete theme
#' @importFrom rlang .data
#' @export
age <- function(
    dl,
    type = "histogram",
    grouping_variable = "study_id"
) {

  dl <- .apply_mapping_to_long_data(dl)


  # ---------------------------
  # Argument validation
  # ---------------------------

  if (!is.data.frame(dl)) {
    stop("`dl` must be a data.frame.", call. = FALSE)
  }

  valid_group_vars <- c(
    "condition_id",
    "study_id",
    "paper_cond_id",
    "paper_study_id"
  )

  if (!grouping_variable %in% valid_group_vars) {
    stop(
      "`grouping_variable` must be one of: ",
      paste(valid_group_vars, collapse = ", "),
      call. = FALSE
    )
  }

  if (!grouping_variable %in% names(dl)) {
    stop(
      "`grouping_variable` not found in `dl`.",
      call. = FALSE
    )
  }

  type <- tolower(type)

  valid_hist <- c("histogram", "hist", "h")
  valid_ridge <- c("ridge", "density", "r", "d")

  if (!type %in% c(valid_hist, valid_ridge)) {
    stop(
      "`type` must be one of: histogram, hist, h, ridge, density, r, d.",
      call. = FALSE
    )
  }

  # ---------------------------
  # Data preparation
  # ---------------------------

  required_cols <- c("measure", "value", "participant_id")
  missing_cols <- setdiff(required_cols, names(dl))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  data_age <- dl |>
    dplyr::filter(.data$measure == "age") |>
    dplyr::select(
      dplyr::any_of(valid_group_vars),
      .data$participant_id,
      .data$value,
      .data$measure
    ) |>
    dplyr::mutate(
      age = suppressWarnings(as.numeric(.data$value)),
      dplyr::across(
        dplyr::any_of(valid_group_vars),
        as.factor
      )
    ) |>
    dplyr::filter(!is.na(.data$age))

  if (nrow(data_age) == 0) {
    stop("No valid age data found after filtering.", call. = FALSE)
  }

  # ---------------------------
  # Order groups by mean age
  # ---------------------------

  study_order <- data_age |>
    dplyr::group_by(.data[[grouping_variable]]) |>
    dplyr::summarise(
      mean_age = mean(.data$age, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$mean_age)) |>
    dplyr::pull(.data[[grouping_variable]])

  data_age[[grouping_variable]] <- factor(
    data_age[[grouping_variable]],
    levels = study_order
  )

  # ---------------------------
  # Plot
  # ---------------------------

  legend_label <- grouping_variable

  if (type %in% valid_hist) {

    data_age <- data_age |>
      dplyr::group_by(
        .data$age,
        .data[[grouping_variable]]
      ) |>
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      )

    graph <- ggplot2::ggplot(
      data_age,
      ggplot2::aes(x = .data$age, y = .data$n)
    ) +
      ggplot2::geom_bar(
        ggplot2::aes(fill = .data[[grouping_variable]]),
        stat = "identity",
        color = "white",
        linewidth = 0.2
      ) +
      ggplot2::labs(
        x = "Age",
        y = "Number of Participants",
        fill = legend_label
      ) +
      ggplot2::scale_fill_discrete(name = legend_label)

  } else {

    graph <- ggplot2::ggplot(
      data_age,
      ggplot2::aes(
        x = .data$age,
        y = .data[[grouping_variable]],
        group = .data[[grouping_variable]],
        fill = .data[[grouping_variable]]
      )
    ) +
      ggridges::geom_density_ridges() +
      ggplot2::labs(
        x = "Age",
        y = legend_label,
        fill = legend_label
      ) +
      ggplot2::theme(
        legend.position = "none"
      )
  }

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
#' @importFrom dplyr filter mutate group_by summarise across all_of
#' @export
ageDescriptives <- function(dl, grouping_variable = NULL) {

  dl <- .apply_mapping_to_long_data(dl)

  # ---------------------------
  # Argument validation
  # ---------------------------

  if (!is.data.frame(dl)) {
    stop("`dl` must be a data.frame.", call. = FALSE)
  }

  required_cols <- c("measure", "value")
  missing_cols <- setdiff(required_cols, names(dl))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(grouping_variable)) {

    if (!is.character(grouping_variable)) {
      stop("`grouping_variable` must be a character vector.", call. = FALSE)
    }

    missing_group_cols <- setdiff(grouping_variable, names(dl))

    if (length(missing_group_cols) > 0) {
      stop(
        "Grouping variable(s) not found in `dl`: ",
        paste(missing_group_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # ---------------------------
  # Data preparation
  # ---------------------------

  df <- dl |>
    dplyr::filter(.data$measure == "age") |>
    dplyr::mutate(
      age = suppressWarnings(as.numeric(.data$value))
    ) |>
    dplyr::filter(!is.na(.data$age))

  if (nrow(df) == 0) {
    stop("No valid age data found after filtering.", call. = FALSE)
  }

  # ---------------------------
  # Optional grouping
  # ---------------------------

  if (!is.null(grouping_variable)) {

    df <- df |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(grouping_variable),
          as.factor
        )
      ) |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(grouping_variable))
      )
  }

  # ---------------------------
  # Summary statistics
  # ---------------------------

  result <- df |>
    dplyr::summarise(
      mean_age = mean(.data$age, na.rm = TRUE),
      sd_age   = stats::sd(.data$age, na.rm = TRUE),
      min_age  = min(.data$age, na.rm = TRUE),
      max_age  = max(.data$age, na.rm = TRUE),
      n        = dplyr::n(),
      .groups  = "drop"
    )

  return(result)
}

# Todo:
# - plot titles
# - (dynamic) axis labels
# - Fix Legend Titles
