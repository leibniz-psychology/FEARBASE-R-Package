#' Visualize contingency instruction counts
#'
#' Creates a horizontal bar plot showing how many studies report each
#' contingency instruction category.
#'
#' The metadata are first passed through the package-internal metadata mapping
#' helper. This allows callers to provide either already mapped metadata with
#' `condition_id` and `study_id`, or metadata that can be mapped from the
#' package's internal condition-to-study mapping.
#'
#' @param md A data frame containing study-level metadata. After internal
#'   mapping, the data must contain the columns `condition_id`, `study_id`, and
#'   `instruction_contingency`.
#'
#' @return A [ggplot2::ggplot()] object with contingency instruction categories
#'   on the y-axis and the number of studies on the x-axis.
#'
#' @examples
#' \dontrun{
#' instructions(metadata)
#' }
#'
#' @importFrom rlang .data
#' @export
instructions <- function(md) {
  # Validate the user-facing input before applying package-internal mapping.
  # This keeps error messages focused on the object supplied by the caller.
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

  # Normalize metadata identifiers so downstream aggregation works for both
  # mapped metadata and raw metadata that only contain the original `id` column.
  md <- .apply_mapping_to_metadata(md)

  # CRAN checks should fail with informative messages instead of surfacing a
  # later tidy-evaluation error from inside the plotting pipeline.
  required_cols <- c("condition_id", "study_id", "instruction_contingency")
  missing_cols <- setdiff(required_cols, names(md))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s) in `md`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Count each instruction category once per study. A study can have multiple
  # mapped conditions, so the distinct study/category table prevents
  # multi-condition studies from inflating study-level frequencies.
  data_instructions <- md |>
    dplyr::select(
      dplyr::all_of(c("condition_id", "study_id")),
      dplyr::starts_with("instruction")
    ) |>
    # Exclude missing instruction values before counting so the plot describes
    # reported contingency-instruction categories only.
    dplyr::filter(!is.na(.data$instruction_contingency)) |>
    dplyr::distinct(
      .data$study_id,
      .data$instruction_contingency
    ) |>
    dplyr::count(.data$instruction_contingency, name = "n") |>
    dplyr::arrange(.data$n) |>
    dplyr::mutate(
      instruction_contingency = factor(
        .data$instruction_contingency,
        levels = .data$instruction_contingency
      )
    )

  # A dedicated empty-data guard gives callers a precise explanation when the
  # metadata contain the required columns but no usable instruction values.
  if (nrow(data_instructions) == 0) {
    stop(
      "No non-missing contingency instruction values were found in `md`.",
      call. = FALSE
    )
  }

  # Build the plot after validation and aggregation are complete. Keeping the
  # plotting layer separate from the data pipeline makes the returned object
  # easier to inspect and test.
  graph <- data_instructions |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$instruction_contingency,
        y = .data$n
      )
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Contingency Instruction",
      y = "Number of Studies"
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(
        from = 0,
        to = max(data_instructions$n),
        by = 2
      )
    )

  return(graph)
}
