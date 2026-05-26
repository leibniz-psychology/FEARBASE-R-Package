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

  # Process Data
  data_age <- dl |>
    filter(measure == "age") |>
    select(
      any_of(c(
        "condition_id",
        "study_id",
        "paper_cond_id",
        "paper_study_id"
      )),
      participant_id,
      value,
      measure
    ) |>
    mutate(
      age = as.numeric(value),
      across(any_of(c(
        "condition_id",
        "study_id",
        "paper_cond_id",
        "paper_study_id"
      )), as.factor)
    ) |>
    filter(!is.na(age))

  # Plot
  study_order <- data_age |>
    group_by(.data[[grouping_variable]]) |>
    summarise(mean_age = mean(age)) |>
    arrange(desc(mean_age)) |>
    pull(.data[[grouping_variable]])

  if (tolower(type) %in% c("histogram", "hist", "h")) {
    data_age <- data_age |>
      group_by(age, .data[[grouping_variable]]) |>
      summarise(n = n())

    data_age[[grouping_variable]] <- factor(
      data_age[[grouping_variable]],
      levels = study_order
    )

    graph <- data_age |>
      ggplot(aes(x = age, y = n)) +
      geom_bar(
        aes(fill = .data[[grouping_variable]]),
        stat = "identity",
        color = "white",
        linewidth = .2
      ) +
      labs(x = "Age", y = "Number of Participants", fill = "Study ID") +
      scale_fill_discrete(name = "Study ID",
                          breaks = rev(study_order))
  }
  else if (tolower(type) %in% c("ridge", "density", "r", "d")) {
    data_age[[grouping_variable]] <- factor(
      data_age[[grouping_variable]],
      levels = study_order
    )

    graph <- data_age |>
      ggplot(aes(
        x = age,
        y = .data[[grouping_variable]],
        group = .data[[grouping_variable]],
        fill = .data[[grouping_variable]]
      )) +
      ggridges::geom_density_ridges() +
      labs(x = "Age", y = "Study ID", fill = "Study ID") +
      theme(legend.position = "none")
  } else {
    stop("unknown argument type")
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

  df <- dl |>
    filter(measure == "age") |>
    mutate(
      age = as.numeric(value)
    ) |>
    filter(!is.na(age))

  # Apply grouping only if provided
  if (!is.null(grouping_variable)) {
    df <- df |>
      mutate(across(all_of(grouping_variable), as.factor)) |>
      group_by(across(all_of(grouping_variable)))
  }

  df |>
    summarise(
      mean_age = mean(age),
      sd = sd(age),
      min = min(age),
      max = max(age),
      .groups = "drop"
    )
}

# Todo:
# - plot titles
# - (dynamic) axis labels
