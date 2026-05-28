#' Visualize contingency instruction counts
#'
#' Creates a horizontal bar plot showing how many studies or conditions report
#' each contingency instruction category.
#'
#' The metadata are first passed through the package-internal metadata mapping
#' helper. This allows callers to provide either already mapped metadata with
#' `condition_id` and `study_id`, or metadata that can be mapped from the
#' package's internal condition-to-study mapping.
#'
#' @param md A data frame containing study-level metadata. After internal
#'   mapping, the data must contain the columns `condition_id`, `study_id`, and
#'   `instruction_contingency`.
#' @param grouping_variable A single character string specifying whether
#'   contingency instruction categories are counted by unique studies or unique
#'   conditions. Must be either `"study_id"` or `"condition_id"`.
#' @param remove_na A single logical value specifying whether rows with missing
#'   `instruction_contingency` values are removed before counting. Defaults to
#'   `TRUE`.
#' @param sort_by_count A single logical value specifying whether instruction
#'   categories are ordered by their observed counts. If `TRUE`, categories are
#'   displayed from highest to lowest count. If `FALSE`, the explicit
#'   instruction order described below is used. Defaults to `FALSE`.
#'
#' @details
#' Instruction categories are displayed from top to bottom in the following
#' order: `"Fully instructed (whole exp)"`,
#' `"Partially instructed (whole exp)"`,
#' `"Different instructions in different conditions"`,
#' `"Different instructions in different phases"`,
#' `"Uninstructed (whole exp)"`, and `"NA"` when `remove_na = FALSE`.
#'
#' @return A [ggplot()] object with contingency instruction categories
#'   on the y-axis and the number of studies or conditions on the x-axis.
#'
#' @examples
#' \dontrun{
#' instructions(metadata)
#' instructions(metadata, grouping_variable = "condition_id")
#' instructions(metadata, remove_na = FALSE)
#' instructions(metadata, sort_by_count = TRUE)
#' }
#'
#' @importFrom rlang .data
#' @export
instructions <- function(
  md,
  grouping_variable = "study_id",
  remove_na = TRUE,
  sort_by_count = FALSE
) {
  ############################################################
  # 1) Validate user-facing inputs before schema normalization
  ############################################################

  # Validate the user-facing input before applying package-internal mapping.
  # This keeps error messages focused on the object supplied by the caller.
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

  # The grouping switch is used for tidy selection and .data pronoun indexing
  # below, so require exactly one explicit supported identifier.
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

  # Limit the public API to the two identifiers created by the metadata mapping
  # helper. This keeps the aggregation semantics clear and testable.
  if (!grouping_variable %in% valid_grouping_variables) {
    stop(
      "`grouping_variable` must be one of: ",
      paste(valid_grouping_variables, collapse = ", "),
      call. = FALSE
    )
  }

  # Missing-instruction handling is a binary counting decision. Require a
  # scalar logical so callers cannot accidentally pass vectors that would make
  # the data pipeline branch ambiguously.
  if (
    !is.logical(remove_na) ||
      length(remove_na) != 1L ||
      is.na(remove_na)
  ) {
    stop(
      "`remove_na` must be a single non-missing logical value.",
      call. = FALSE
    )
  }

  # Sorting by count is an optional presentation choice. Require a scalar
  # logical value so category ordering cannot branch ambiguously.
  if (
    !is.logical(sort_by_count) ||
      length(sort_by_count) != 1L ||
      is.na(sort_by_count)
  ) {
    stop(
      "`sort_by_count` must be a single non-missing logical value.",
      call. = FALSE
    )
  }

  ############################################################
  # 2) Apply FEARBASE metadata mapping and validate required columns
  ############################################################

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

  ############################################################
  # 3) Count each instruction category by the requested grouping identifier
  ############################################################

  # Count each instruction category once per requested grouping identifier.
  # For study-level counts this prevents multi-condition studies from inflating
  # frequencies; for condition-level counts each condition contributes at most
  # once to its reported contingency-instruction category.
  data_instructions <- md |>
    select(
      all_of(c("condition_id", "study_id")),
      starts_with("instruction")
    )

  # By default the plot describes reported contingency-instruction categories
  # only. When remove_na = FALSE, missing instruction rows are retained and
  # counted as their own missing category in the returned plot data.
  if (remove_na) {
    data_instructions <- data_instructions |>
      filter(!is.na(.data$instruction_contingency))
  }

  data_instructions <- data_instructions |>
    # De-duplicate before counting so repeated rows for the same study or
    # condition do not inflate the selected aggregation level.
    distinct(
      .data[[grouping_variable]],
      .data$instruction_contingency
    ) |>
    count(.data$instruction_contingency, name = "n")

  # Define the visual top-to-bottom category order requested for this plot.
  instruction_order <- c(
    "Fully instructed (whole exp)",
    "Partially instructed (whole exp)",
    "Different instructions in different conditions",
    "Different instructions in different phases",
    "Uninstructed (whole exp)"
  )

  # Convert missing categories to an explicit display label when requested so
  # the missing-value bar can be positioned intentionally at the bottom of the
  # flipped plot rather than left to ggplot2's default missing-value placement.
  if (!remove_na) {
    data_instructions <- data_instructions |>
      mutate(
        instruction_contingency = if_else(
          is.na(.data$instruction_contingency),
          "NA",
          as.character(.data$instruction_contingency)
        )
      )

    instruction_order <- c(instruction_order, "NA")
  }

  if (sort_by_count) {
    # For horizontal bars, highest-count categories should be easiest to scan at
    # the top. Ties fall back to alphabetical order for deterministic output.
    display_order <- data_instructions |>
      arrange(
        desc(.data$n),
        .data$instruction_contingency
      ) |>
      pull("instruction_contingency") |>
      as.character()
  } else {
    # Keep any unexpected but valid metadata labels in the plot after the known
    # categories and before the explicit missing-value label.
    unexpected_instruction_order <- setdiff(
      as.character(data_instructions$instruction_contingency),
      instruction_order
    )
    display_order <- c(
      setdiff(instruction_order, "NA"),
      unexpected_instruction_order,
      intersect("NA", instruction_order)
    )
  }

  # Factor levels are reversed because coord_flip() renders the first level at
  # the bottom and the last level at the top.

  data_instructions <- data_instructions |>
    mutate(
      instruction_contingency = factor(
        .data$instruction_contingency,
        levels = rev(display_order)
      )
    ) |>
    arrange(.data$instruction_contingency)

  # A dedicated empty-data guard gives callers a precise explanation when the
  # metadata contain the required columns but no usable instruction values.
  if (nrow(data_instructions) == 0) {
    stop(
      "No non-missing contingency instruction values were found in `md`.",
      call. = FALSE
    )
  }

  ############################################################
  # 4) Build and return the requested horizontal bar plot
  ############################################################

  # coord_flip() renders the count scale as the visual x-axis, while ggplot2
  # keeps that scale attached to the y aesthetic. The label therefore lives in
  # labs(y = ...) so the drawn horizontal count axis has the requested title.
  count_axis_title <- if (identical(grouping_variable, "study_id")) {
    "Number of Studies"
  } else {
    "Number of Conditions"
  }

  # Build the plot after validation and aggregation are complete. Keeping the
  # plotting layer separate from the data pipeline makes the returned object
  # easier to inspect and test.
  graph <- data_instructions |>
    ggplot(
      aes(
        x = .data$instruction_contingency,
        y = .data$n
      )
    ) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      x = "Contingency Instruction",
      y = count_axis_title
    ) +
    scale_y_continuous(
      breaks = seq(
        from = 0,
        to = max(data_instructions$n),
        by = 2
      )
    )

  return(graph)
}
