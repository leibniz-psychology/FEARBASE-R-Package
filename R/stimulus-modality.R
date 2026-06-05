#' Visualize Stimulus Modality Distributions
#'
#' Creates a pie chart showing the distribution of unconditioned stimulus (US)
#' or conditioned stimulus (CS) modalities reported in FEARBASE study metadata.
#'
#' Metadata are first passed through the package-internal study-to-condition
#' mapping helper so metadata exposes the same
#' `condition_id` and `study_id` columns before modality counts are computed.
#'
#' @param md A data frame containing study metadata. After internal metadata
#'   mapping, the data frame must contain `condition_id`, `study_id`,
#'   `n_subjects`, and the modality column requested with `type`. If `NULL`,
#'   the function first attempts to use an object named `metadata` from the
#'   calling environment.
#' @param type Character string specifying which stimulus modality to plot.
#'   Must be exactly one of:
#'   \itemize{
#'     \item `"us_type"`: unconditioned stimulus modality.
#'     \item `"cs_type"`: conditioned stimulus modality.
#'   }
#' @param level Character string specifying the aggregation level used for pie
#'   slice sizes. Must be exactly one of:
#'   \itemize{
#'     \item `"n_studies"`: number of distinct studies per modality.
#'     \item `"n_subjects"`: sum of participants per modality.
#'   }
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Resolves the metadata source if `md = NULL`.
#'   \item Validates that `md` is a data frame.
#'   \item Applies `.apply_mapping_to_metadata()` to normalize identifier
#'     columns.
#'   \item Validates that all columns required for the selected modality and
#'     aggregation level are present after mapping.
#'   \item Converts non-missing `n_subjects` values to numeric values.
#'   \item Replaces missing or empty modality values with `"not reported"`.
#'   \item Aggregates either distinct studies or participant counts per
#'     modality.
#'   \item Returns a `ggplot2` pie chart.
#' }
#'
#' Non-missing values in `n_subjects` must be numeric or coercible to numeric.
#' Missing participant counts are treated as zero when `level = "n_subjects"`.
#' The function raises an error if the selected aggregation level contains no
#' positive observations after validation.
#'
#' @return A `ggplot2` object showing modality counts for the selected
#'   aggregation level.
#'
#' @examples
#' \dontrun{
#' stimulus_modality(metadata, type = "us_type", level = "n_studies")
#' stimulus_modality(metadata, type = "cs_type", level = "n_subjects")
#' }
#'
#' @importFrom rlang .data
#' @export
stimulus_modality <- function(
  md = NULL,
  type = "us_type",
  level = "n_studies"
) {
  ############################################################
  # 1) Resolve the metadata source
  ############################################################

  # Resolve explicit data or a caller-side metadata object only. Ignored local
  # data files are intentionally not read by package code.
  md <- .resolve_metadata(md, caller_env = parent.frame())

  ############################################################
  # 2) Validate scalar function arguments
  ############################################################

  # The tidy evaluation below expects one column name for the modality and one
  # count column for slice size. Reject vectors, missing values, and non-string
  # inputs before any data transformation.
  # Keep supported modality columns explicit so typo-related errors are
  # reported as argument problems rather than later dplyr column errors.
  valid_types <- c("us_type", "cs_type")
  type <- .validate_choice(type, "type", valid_types)

  # These are the only aggregations the plotting data frame creates below.
  valid_levels <- c("n_studies", "n_subjects")
  level <- .validate_choice(level, "level", valid_levels)

  ############################################################
  # 3) Validate and normalize the metadata schema
  ############################################################

  # Mapping, column selection, and plotting all require a rectangular object
  # with named columns, so fail early for unsupported input classes.
  .validate_data_frame(md, "md")

  # Normalize metadata identifiers before checking required columns so callers
  # can provide either already mapped metadata or legacy FEARBASE metadata.
  md <- .apply_mapping_to_metadata(md)

  # The requested modality and both count fields are prepared from this compact
  # schema. Checking all columns at once makes malformed inputs easier to fix.
  required_cols <- c("condition_id", "study_id", "n_subjects", type)
  .validate_required_columns(md, required_cols, "md")

  ############################################################
  # 4) Prepare modality labels and participant counts
  ############################################################

  # Coerce participant counts through character to avoid factor integer-code
  # coercion. Numeric vectors pass through unchanged after as.character().
  n_subjects <- .coerce_numeric_strict(md$n_subjects, "`md$n_subjects`")

  # Store the validated numeric counts on the mapped metadata. Missing counts
  # are converted to zero so participant totals do not become NA.
  md$n_subjects <- coalesce(n_subjects, 0)

  # Build a compact plotting table with one row per condition-level metadata
  # record. Missing or blank modality labels are counted explicitly instead of
  # disappearing from the visualization.
  data_modality <- md |>
    select(
      all_of(c("condition_id", "study_id", "n_subjects", type))
    ) |>
    mutate(
      modality = stringr::str_squish(as.character(.data[[type]])),
      modality = if_else(
        is.na(.data$modality) | .data$modality == "",
        "not reported",
        .data$modality
      )
    )

  ############################################################
  # 5) Aggregate modality counts for the selected level
  ############################################################

  # Aggregate both supported levels in one summary table. Distinct study IDs
  # keep n_studies aligned with its name, while n_subjects sums the validated
  # participant counts across metadata rows for each modality.
  count_data <- data_modality |>
    group_by(.data$modality) |>
    summarise(
      n_studies = n_distinct(.data$study_id),
      n_subjects = sum(.data$n_subjects),
      .groups = "drop"
    ) |>
    arrange(desc(.data[[level]]), .data$modality) |>
    mutate(
      modality = stringr::str_replace(
        .data$modality,
        pattern = ",",
        replacement = ", "
      )
    ) |>
    mutate(
      modality = factor(.data$modality, levels = .data$modality)
    )

  # An all-zero chart would either be empty or visually misleading, especially
  # for participant counts where all source values may have been missing.
  if (sum(count_data[[level]]) <= 0) {
    stop(
      "`md` must contain at least one positive observation for `level = \"",
      level,
      "\"`.",
      call. = FALSE
    )
  }

  # Labels belong only to positive slices. This avoids placing labels on zero
  # categories if future data preparation preserves absent factor levels.
  label_data <- count_data |>
    filter(.data[[level]] > 0)

  ############################################################
  # 6) Build and return the ggplot object
  ############################################################

  # Use human-readable legend and label text while keeping argument values as
  # stable programmatic identifiers for callers.
  legend_title <- if (type == "us_type") {
    "US Modality"
  } else {
    "CS Modality"
  }

  count_label <- if (level == "n_studies") {
    "Studies"
  } else {
    "Participants"
  }

  # The package discrete palette starts multi-category scales with the
  # secondary blue, while generate_palette(1) returns the primary dark blue.
  # Because count_data is ordered by descending selected count, placing the
  # primary color first restores the original visual contract: the largest
  # modality slice receives the main FEARBASE color.
  n_modalities <- nlevels(count_data$modality)
  main_color <- generate_palette(1)
  generated_colors <- generate_palette(max(n_modalities, 2L))
  secondary_colors <- generated_colors[
    tolower(generated_colors) != tolower(main_color)
  ]
  fill_values <- c(main_color, secondary_colors)[seq_len(n_modalities)]
  names(fill_values) <- levels(count_data$modality)

  # Build a one-column stacked bar and project it into polar coordinates,
  # which is ggplot2's standard pie-chart construction.
  graph <- ggplot(
    count_data,
    aes(
      x = "",
      y = .data[[level]],
      fill = .data$modality
    )
  ) +
    geom_col(width = 1) +
    coord_polar("y", start = 0) +
    labs(fill = legend_title) +
    scale_fill_manual(values = fill_values) +
    theme_fearbase_void(background = "white") +
    geom_label(
      data = label_data,
      aes(
        label = paste0(.data$modality, " (", .data[[level]], ")"),
        group = .data$modality
      ),
      position = position_stack(vjust = 0.5),
      fill = "white",
      size = .fearbase_geom_text_size(scale = 0.85)
    ) +
    guides(
      fill = guide_none()
      #fill = guide_legend(title = paste0(legend_title, " by ", count_label))
    )

  # Return the plot without printing so callers can add layers, themes, or save
  # it with ggsave().
  return(graph)
}
