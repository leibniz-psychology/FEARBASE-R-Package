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
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

  md <- .apply_mapping_to_metadata(md)

  required_cols <- c("condition_id", "study_id", "instruction_contingency")
  missing_cols <- setdiff(required_cols, names(md))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s) in `md`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Count each instruction category once per study so multi-condition studies do
  # not inflate the study-level frequency displayed in the graph.
  data_instructions <- md |>
    dplyr::select(
      dplyr::all_of(c("condition_id", "study_id")),
      dplyr::starts_with("instruction")
    ) |>
    #dplyr::filter(!is.na(.data$instruction_contingency)) |>
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

  if (nrow(data_instructions) == 0) {
    stop(
      "No non-missing contingency instruction values were found in `md`.",
      call. = FALSE
    )
  }

  # Build the plot after all data validation and counting are complete. Keeping
  # plotting separate from aggregation makes the returned object easier to test.
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
    )

  return(graph)
}