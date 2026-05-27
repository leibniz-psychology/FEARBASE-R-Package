#' Visualize Stimulus Modality Distributions
#'
#' Creates a pie chart showing the distribution of unconditioned stimulus (US)
#' or conditioned stimulus (CS) modalities reported in FEARBASE study metadata.
#'
#' Metadata are first passed through the package-internal study-to-condition
#' mapping helper so current and legacy metadata schemas expose the same
#' `condition_id` and `study_id` columns before modality counts are computed.
#'
#' @param md A data frame containing study metadata. After internal metadata
#'   mapping, the data frame must contain `condition_id`, `study_id`,
#'   `n_subjects`, and the modality column requested with `type`. If `NULL`,
#'   the function first attempts to use an object named `metadata` from the
#'   calling environment and then falls back to the package-bundled
#'   `data/metadata.csv` file.
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
#' stimModality(metadata, type = "us_type", level = "n_studies")
#' stimModality(metadata, type = "cs_type", level = "n_subjects")
#' }
#'
#' @importFrom rlang .data
#' @export
stimModality <- function(
    md = NULL,
    type = "us_type",
    level = "n_studies"
) {
  ############################################################
  # 1) Resolve the metadata source
  ############################################################

  # Let interactive users call stimModality() without an explicit data frame
  # while keeping the explicit `md` argument as the reproducible primary path.
  if (is.null(md)) {
    # Prefer a caller-side metadata object because it is likely the active data
    # set the user is currently exploring or validating.
    if (exists("metadata", envir = parent.frame(), inherits = TRUE)) {
      md <- get("metadata", envir = parent.frame(), inherits = TRUE)
    } else {
      # Resolve the package data path lazily so a missing bundled metadata file
      # can be reported with a direct package-specific error below.
      metadata_path <- system.file(
        "data",
        "metadata.csv",
        package = "fearbase",
        mustWork = FALSE
      )
    }

    # Stop before readr receives an empty path. That keeps the error message
    # about the function contract rather than the file system.
    if (is.null(md) && identical(metadata_path, "")) {
      stop(
        "`md` must be supplied, an object named `metadata` must exist ",
        "in the calling environment, or bundled metadata must be available.",
        call. = FALSE
      )
    }

    if (is.null(md)) {
      # Use bundled metadata only as a final convenience fallback for examples,
      # tests, and interactive calls where no explicit data object was supplied.
      md <- readr::read_csv(metadata_path, show_col_types = FALSE)
    }
  }

  ############################################################
  # 2) Validate scalar function arguments
  ############################################################

  # The tidy evaluation below expects one column name for the modality and one
  # count column for slice size. Reject vectors, missing values, and non-string
  # inputs before any data transformation.
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    stop("`type` must be a single character string.", call. = FALSE)
  }

  # Keep supported modality columns explicit so typo-related errors are
  # reported as argument problems rather than later dplyr column errors.
  valid_types <- c("us_type", "cs_type")

  if (!type %in% valid_types) {
    stop(
      "`type` must be one of: ",
      paste(valid_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # Validate `level` with the same scalar-character contract used for `type`.
  if (!is.character(level) || length(level) != 1L || is.na(level)) {
    stop("`level` must be a single character string.", call. = FALSE)
  }

  # These are the only aggregations the plotting data frame creates below.
  valid_levels <- c("n_studies", "n_subjects")

  if (!level %in% valid_levels) {
    stop(
      "`level` must be one of: ",
      paste(valid_levels, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  ############################################################
  # 3) Validate and normalize the metadata schema
  ############################################################

  # Mapping, column selection, and plotting all require a rectangular object
  # with named columns, so fail early for unsupported input classes.
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

  # Normalize metadata identifiers before checking required columns so callers
  # can provide either already mapped metadata or legacy FEARBASE metadata.
  md <- .apply_mapping_to_metadata(md)

  # The requested modality and both count fields are prepared from this compact
  # schema. Checking all columns at once makes malformed inputs easier to fix.
  required_cols <- c("condition_id", "study_id", "n_subjects", type)
  missing_cols <- setdiff(required_cols, names(md))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ############################################################
  # 4) Prepare modality labels and participant counts
  ############################################################

  # Coerce participant counts through character to avoid factor integer-code
  # coercion. Numeric vectors pass through unchanged after as.character().
  n_subjects <- suppressWarnings(as.numeric(as.character(md$n_subjects)))

  # Only supplied, non-missing values that cannot become numeric are invalid.
  # Missing counts are allowed because they can be treated as zero for sums.
  invalid_n_subjects <- !is.na(md$n_subjects) & is.na(n_subjects)

  if (any(invalid_n_subjects)) {
    stop(
      "All non-missing values in `md$n_subjects` must be numeric or ",
      "coercible to numeric.",
      call. = FALSE
    )
  }

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
  n_modalities <- length(levels(count_data$modality))
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
    theme_void() +
    geom_label(
      data = label_data,
      aes(
        label = paste0(.data$modality, " (", .data[[level]], ")"),
        group = .data$modality
      ),
      position = position_stack(vjust = 0.5),
      fill = "white"
    ) +
    theme(
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    guides(
      fill = guide_legend(title = paste0(legend_title, " by ", count_label))
    )

  # Return the plot without printing so callers can add layers, themes, or save
  # it with ggsave().
  return(graph)
}
