#' Visualize SCR peak detection windows
#'
#' Creates a horizontal interval plot of skin conductance response (SCR)
#' baseline, peak-detection, and trough-detection scoring windows by study or
#' condition.
#'
#' The metadata are first passed through the package-internal metadata mapping
#' helper. The function supports both the current `physio_scr_*` SCR window
#' column names and the legacy `scr_*` column names.
#'
#' @param md A data frame containing study-level metadata. After internal
#'   mapping, the data must contain at least one supported grouping column
#'   (`study_id` or `condition_id`) and one complete supported SCR window column
#'   set. See Details.
#' @param grouping_variable A single character string specifying the grouping
#'   variable shown on the discrete axis. Must be either `"study_id"` or
#'   `"condition_id"`. If `"condition_id"` is requested but only `study_id` is
#'   available after mapping, the function falls back to `study_id`.
#'
#' @details
#' The current supported SCR window column set is:
#' \itemize{
#'   \item `physio_scr_scoring_approach`
#'   \item `physio_scr_baseline_window_start`
#'   \item `physio_scr_baseline_window_end`
#'   \item `physio_scr_peak_detection_window_min`
#'   \item `physio_scr_peak_detection_window_max`
#' }
#'
#' The legacy supported SCR window column set is:
#' \itemize{
#'   \item `scr_scoring_approach`
#'   \item `scr_baseline_window_start`
#'   \item `scr_baseline_window_end`
#'   \item `scr_peak_detection_window_min`
#'   \item `scr_peak_detection_window_max`
#' }
#'
#' For trough-to-peak scoring, baseline windows are removed before plotting
#' because this scoring approach uses trough detection instead of a baseline
#' correction interval.
#'
#' @return A [ggplot()] object showing SCR scoring windows in seconds
#'   relative to stimulus onset.
#'
#' @examples
#' \dontrun{
#' peakDetectionWindows(metadata)
#' peakDetectionWindows(metadata, grouping_variable = "condition_id")
#' }
#'
#' @importFrom rlang .data
#' @export
peakDetectionWindows <- function(
  md,
  grouping_variable = "study_id"
) {
  ############################################################
  # 1) Validate user inputs before touching package internals
  ############################################################

  # Downstream dplyr and tidyr calls require rectangular metadata with named
  # columns, so fail early if the caller supplies another object type.
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

  # Require exactly one non-missing grouping column name. This keeps later
  # tidy-selection and .data pronoun lookups deterministic.
  if (
    !is.character(grouping_variable) ||
      length(grouping_variable) != 1 ||
      is.na(grouping_variable)
  ) {
    stop(
      "`grouping_variable` must be a single non-missing character string.",
      call. = FALSE
    )
  }

  valid_grouping_variables <- c("study_id", "condition_id")

  # The plot is intentionally limited to the two package-level identifiers
  # created by the metadata mapping helper.
  if (!grouping_variable %in% valid_grouping_variables) {
    stop(
      "`grouping_variable` must be one of: ",
      paste(valid_grouping_variables, collapse = ", "),
      call. = FALSE
    )
  }

  ############################################################
  # 2) Apply FEARBASE metadata mapping and resolve grouping
  ############################################################

  # Add or normalize study_id and condition_id columns using the package's
  # central mapping logic before deciding what can be plotted.
  md <- .apply_mapping_to_metadata(md)

  # Keep only grouping variables that are actually available after mapping.
  available_grouping_variables <- intersect(valid_grouping_variables, names(md))

  if (length(available_grouping_variables) == 0) {
    stop(
      "No supported grouping variable is available in `md` after mapping.",
      call. = FALSE
    )
  }

  # If condition-level grouping was requested but mapped metadata only contains
  # study_id, fall back to study-level plotting. Other missing grouping requests
  # cannot be represented on the discrete axis and should stop with guidance.
  if (!grouping_variable %in% available_grouping_variables) {
    can_fallback_to_study <- identical(grouping_variable, "condition_id") &&
      "study_id" %in% names(md)

    if (can_fallback_to_study) {
      grouping_variable <- "study_id"
    } else {
      stop(
        "`grouping_variable` must be one of the available columns: ",
        paste(available_grouping_variables, collapse = ", "),
        call. = FALSE
      )
    }
  }

  ############################################################
  # 3) Resolve supported SCR metadata schema
  ############################################################

  # Current metadata uses physio_scr_* columns. The vector names are internal
  # aliases used below to rename whichever supported schema was supplied.
  # TODO: Remove legacy support after everything is properly in new "format"
  current_scr_cols <- c(
    scr_scoring_col = "physio_scr_scoring_approach",
    scr_baseline_start_col = "physio_scr_baseline_window_start",
    scr_baseline_end_col = "physio_scr_baseline_window_end",
    scr_peak_min_col = "physio_scr_peak_detection_window_min",
    scr_peak_max_col = "physio_scr_peak_detection_window_max"
  )

  # Legacy metadata used shorter scr_* column names. Retaining this mapping
  # preserves compatibility with older FEARBASE metadata exports.
  legacy_scr_cols <- c(
    scr_scoring_col = "scr_scoring_approach",
    scr_baseline_start_col = "scr_baseline_window_start",
    scr_baseline_end_col = "scr_baseline_window_end",
    scr_peak_min_col = "scr_peak_detection_window_min",
    scr_peak_max_col = "scr_peak_detection_window_max"
  )

  # Select one complete SCR schema. Partial schemas are rejected because missing
  # endpoints would create incomplete or misleading scoring-window intervals.
  if (all(current_scr_cols %in% names(md))) {
    scr_cols <- current_scr_cols
  } else if (all(legacy_scr_cols %in% names(md))) {
    scr_cols <- legacy_scr_cols
  } else {
    stop(
      "The metadata do not contain a complete supported set of SCR window ",
      "columns.",
      call. = FALSE
    )
  }

  ############################################################
  # 4) Normalize, clean, and reshape SCR scoring windows
  ############################################################

  # Normalize the supported current or legacy SCR schema into one internal
  # column layout before reshaping. This keeps the plotting code independent of
  # which metadata schema the caller supplied.
  data_peak_detection_window <- md |>
    # Retain only grouping IDs and SCR window fields so distinct() later
    # de-duplicates plotting rows rather than unrelated metadata columns.
    select(
      all_of(available_grouping_variables),
      all_of(unname(scr_cols))
    ) |>
    # Rename the chosen current or legacy columns to one stable internal schema.
    rename(
      scr_scoring_approach = all_of(scr_cols[["scr_scoring_col"]]),
      scr_baseline_window_start = all_of(
        scr_cols[["scr_baseline_start_col"]]
      ),
      scr_baseline_window_end = all_of(
        scr_cols[["scr_baseline_end_col"]]
      ),
      scr_peak_detection_window_min = all_of(
        scr_cols[["scr_peak_min_col"]]
      ),
      scr_peak_detection_window_max = all_of(
        scr_cols[["scr_peak_max_col"]]
      )
    ) |>
    # Coerce all window endpoints to numeric seconds. Non-numeric values become
    # NA and are removed after the interval structure has been reshaped.
    mutate(
      across(
        all_of(c(
          "scr_baseline_window_start",
          "scr_baseline_window_end",
          "scr_peak_detection_window_min",
          "scr_peak_detection_window_max"
        )),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) |>
    # Keep rows that at least identify a scoring approach and the upper
    # detection-window endpoint before constructing candidate intervals.
    tidyr::drop_na(
      all_of(c("scr_scoring_approach", "scr_peak_detection_window_max"))
    ) |>
    # Remove duplicate plotting definitions that can appear in repeated
    # metadata rows or after retaining both study_id and condition_id.
    distinct() |>
    # Sort similar scoring approaches and detection windows together before
    # factor conversion and plotting.
    arrange(
      .data$scr_scoring_approach,
      desc(.data$scr_peak_detection_window_min),
      desc(.data$scr_peak_detection_window_max)
    ) |>
    # Store grouping IDs as factors so ggplot treats them as discrete groups.
    mutate(
      across(all_of(available_grouping_variables), as.factor)
    ) |>
    # Convert endpoint columns into long form. For example,
    # scr_baseline_window_start becomes measure = scr, window = baseline,
    # and timepoint = start.
    tidyr::pivot_longer(
      cols = -all_of(c(
        available_grouping_variables,
        "scr_scoring_approach"
      )),
      names_to = c("measure", "window", "timepoint"),
      names_pattern = "(scr)_(.*)_window_(.*)"
    ) |>
    # Harmonize peak min/max endpoint names with baseline start/end names so
    # pivot_wider() can build shared start and end columns for every window.
    mutate(
      timepoint = forcats::fct_recode(
        .data$timepoint,
        start = "min",
        end = "max"
      )
    ) |>
    # Create one interval row per group, scoring approach, and window type.
    tidyr::pivot_wider(
      names_from = "timepoint",
      values_from = "value"
    ) |>
    # Trough-to-peak scoring uses trough detection rather than baseline
    # correction, so baseline rows for that scoring approach are not plotted.
    filter(
      !(.data$scr_scoring_approach == "trough-to-peak" &
        .data$window == "baseline")
    ) |>
    # Recode compact plot labels. BLC rows keep baseline and peak-detection
    # windows, while TTP rows are labeled as trough-detection windows.
    mutate(
      scr_scoring_approach = forcats::fct_recode(
        .data$scr_scoring_approach,
        "BLC" = "baseline_correction",
        "TTP" = "trough-to-peak"
      ),
      # TODO: Muss angepasst werden, wenn wir Auswahloptionen im Scoring Approach Drop-Down anpassen
      window = case_when(
        .data$scr_scoring_approach != "BLC" ~ "Trough Detection",
        .data$scr_scoring_approach == "BLC" &
          .data$window == "peak_detection" ~ "Peak Detection",
        .data$scr_scoring_approach == "BLC" &
          .data$window == "baseline" ~ "Baseline",
        TRUE ~ .data$window
      ) |>
        factor(levels = c("Baseline", "Peak Detection", "Trough Detection"))
    ) |>
    # Only complete numeric intervals can be drawn as geom_segment() rows.
    tidyr::drop_na(all_of(c("start", "end")))

  # Supported columns can still produce no plottable rows if all endpoint values
  # were missing or non-numeric, so give a data-quality error before ggplot.
  if (nrow(data_peak_detection_window) == 0) {
    stop(
      "No complete SCR peak detection window rows were found in `md`.",
      call. = FALSE
    )
  }

  ############################################################
  # 5) Compute stable discrete-axis order
  ############################################################

  # Build the discrete plotting order once so every layer uses the same group
  # positions. The first key keeps BLC rows below TTP rows after coord_flip();
  # the second key orders groups within each scoring approach by their earliest
  # plotted window start.
  group_axis_levels <- data_peak_detection_window |>
    # Collapse multiple interval rows per study/condition into a single
    # ordering record for the discrete plot axis.
    group_by(.data[[grouping_variable]]) |>
    summarise(
      scr_scoring_approach_order = min(
        as.integer(.data$scr_scoring_approach),
        na.rm = TRUE
      ),
      start_order = min(.data$start, na.rm = TRUE),
      end_order = max(.data$end, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(
      .data$scr_scoring_approach_order,
      .data$start_order,
      .data$end_order,
      .data[[grouping_variable]]
    ) |>
    # Pull ordered IDs as character values so they can become explicit factor
    # levels for the plotting-only group variable.
    pull(all_of(grouping_variable)) |>
    as.character()

  # Use a dedicated plotting factor to preserve the original grouping column
  # while still forcing every ggplot layer to share the computed axis order.
  data_peak_detection_window <- data_peak_detection_window |>
    mutate(
      plot_group = factor(
        as.character(.data[[grouping_variable]]),
        levels = group_axis_levels
      )
    )

  ############################################################
  # 6) Compute continuous-axis limits, breaks, and annotation positions
  ############################################################

  # Extend the lower limit by one second so the scoring-approach labels can sit
  # inside the panel without overlapping the first plotted interval.
  y_limits <- range(
    c(data_peak_detection_window$start - 1, data_peak_detection_window$end),
    na.rm = TRUE
  )

  # Guard against all-missing or infinite endpoint values after numeric
  # coercion. This produces a clearer message than a later ggplot scale error.
  if (any(!is.finite(y_limits))) {
    stop(
      "SCR window start and end values must contain finite numeric data.",
      call. = FALSE
    )
  }

  # Use whole-second tick marks over the observed window range.
  y_breaks <- seq(
    floor(min(data_peak_detection_window$start, na.rm = TRUE)),
    ceiling(max(data_peak_detection_window$end, na.rm = TRUE)),
    by = 1
  )

  # Count plotted groups after dropping unused factor levels so label placement
  # adapts to the actual chart height.
  n_groups <- nlevels(droplevels(data_peak_detection_window[[
    grouping_variable
  ]]))

  # Position the CS onset label close to the top of the flipped panel, with a
  # minimum one-group offset for very small datasets.
  cs_onset_label_x <- n_groups - max(1, round(n_groups * 0.1))

  ############################################################
  # 7) Build and return the ggplot object
  ############################################################

  # Build the plot with group on x and time on y, then flip coordinates so SCR
  # scoring windows are displayed as horizontal intervals.
  graph <- data_peak_detection_window |>
    ggplot(
      aes(
        x = .data$plot_group
      )
    ) +
    # Draw baseline, peak-detection, and trough-detection windows as thick
    # horizontal segments after coord_flip().
    geom_segment(
      aes(
        y = .data$start,
        yend = .data$end,
        color = .data$window,
        group = .data$window
      ),
      linewidth = 7
    ) +
    # Label the final flipped plot: studies/conditions on the vertical axis and
    # time relative to stimulus onset on the horizontal axis.
    labs(
      x = "Study",
      y = "Time (s) relative to stimulus onset",
      color = "Window:"
    ) +
    # Flip axes and apply the expanded time limits computed above.
    coord_flip(ylim = y_limits) +
    # Add BLC/TTP scoring labels at the left edge of each plotted group.
    geom_text(
      aes(
        y = min(y_limits),
        label = .data$scr_scoring_approach
      ),
      color = "black",
      hjust = 0,
      size = 3,
      fontface = 2
    ) +
    # Draw a zero-second reference line for stimulus onset.
    geom_hline(yintercept = 0) +
    # Label stimulus onset once instead of repeating it for every group.
    geom_text(
      x = cs_onset_label_x,
      y = 0,
      angle = 90,
      label = "CS Onset",
      color = "black",
      vjust = -0.5,
      size = 5
    ) +
    # Apply whole-second breaks and keep the legend above the plotting panel.
    scale_y_continuous(breaks = y_breaks) +
    theme(legend.position = "top")

  # Return the ggplot object so callers can print it, add layers, or save it.
  return(graph)
}
