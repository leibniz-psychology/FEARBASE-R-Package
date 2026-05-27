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
#' @return A [ggplot2::ggplot()] object showing SCR scoring windows in seconds
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
  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

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

  if (!grouping_variable %in% valid_grouping_variables) {
    stop(
      "`grouping_variable` must be one of: ",
      paste(valid_grouping_variables, collapse = ", "),
      call. = FALSE
    )
  }

  md <- .apply_mapping_to_metadata(md)

  available_grouping_variables <- intersect(valid_grouping_variables, names(md))

  if (length(available_grouping_variables) == 0) {
    stop(
      "No supported grouping variable is available in `md` after mapping.",
      call. = FALSE
    )
  }

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

  current_scr_cols <- c(
    scr_scoring_col = "physio_scr_scoring_approach",
    scr_baseline_start_col = "physio_scr_baseline_window_start",
    scr_baseline_end_col = "physio_scr_baseline_window_end",
    scr_peak_min_col = "physio_scr_peak_detection_window_min",
    scr_peak_max_col = "physio_scr_peak_detection_window_max"
  )

  legacy_scr_cols <- c(
    scr_scoring_col = "scr_scoring_approach",
    scr_baseline_start_col = "scr_baseline_window_start",
    scr_baseline_end_col = "scr_baseline_window_end",
    scr_peak_min_col = "scr_peak_detection_window_min",
    scr_peak_max_col = "scr_peak_detection_window_max"
  )

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

  # Normalize the supported current or legacy SCR schema into one internal
  # column layout before reshaping. This keeps the plotting code independent of
  # which metadata schema the caller supplied.
  data_peak_detection_window <- md |>
    dplyr::select(
      dplyr::all_of(available_grouping_variables),
      dplyr::all_of(unname(scr_cols))
    ) |>
    dplyr::rename(
      scr_scoring_approach = dplyr::all_of(scr_cols[["scr_scoring_col"]]),
      scr_baseline_window_start = dplyr::all_of(
        scr_cols[["scr_baseline_start_col"]]
      ),
      scr_baseline_window_end = dplyr::all_of(
        scr_cols[["scr_baseline_end_col"]]
      ),
      scr_peak_detection_window_min = dplyr::all_of(
        scr_cols[["scr_peak_min_col"]]
      ),
      scr_peak_detection_window_max = dplyr::all_of(
        scr_cols[["scr_peak_max_col"]]
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "scr_baseline_window_start",
          "scr_baseline_window_end",
          "scr_peak_detection_window_min",
          "scr_peak_detection_window_max"
        )),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) |>
    tidyr::drop_na(
      dplyr::all_of(c("scr_scoring_approach", "scr_peak_detection_window_max"))
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(
      .data$scr_scoring_approach,
      dplyr::desc(.data$scr_peak_detection_window_min),
      dplyr::desc(.data$scr_peak_detection_window_max)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(available_grouping_variables), as.factor)
    ) |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(c(
        available_grouping_variables,
        "scr_scoring_approach"
      )),
      names_to = c("measure", "window", "timepoint"),
      names_pattern = "(scr)_(.*)_window_(.*)"
    ) |>
    dplyr::mutate(
      timepoint = forcats::fct_recode(
        .data$timepoint,
        start = "min",
        end = "max"
      )
    ) |>
    tidyr::pivot_wider(
      names_from = "timepoint",
      values_from = "value"
    ) |>
    dplyr::filter(
      !(
        .data$scr_scoring_approach == "trough-to-peak" &
          .data$window == "baseline"
      )
    ) |>
    dplyr::mutate(
      scr_scoring_approach = forcats::fct_recode(
        .data$scr_scoring_approach,
        "BLC" = "baseline_correction",
        "TTP" = "trough-to-peak"
      ),
      window = dplyr::case_when(
        .data$scr_scoring_approach != "BLC" ~ "Trough Detection",
        .data$scr_scoring_approach == "BLC" &
          .data$window == "peak_detection" ~ "Peak Detection",
        .data$scr_scoring_approach == "BLC" &
          .data$window == "baseline" ~ "Baseline",
        TRUE ~ .data$window
      ) |>
        factor(levels = c("Baseline", "Peak Detection", "Trough Detection"))
    ) |>
    tidyr::drop_na(dplyr::all_of(c("start", "end")))

  if (nrow(data_peak_detection_window) == 0) {
    stop(
      "No complete SCR peak detection window rows were found in `md`.",
      call. = FALSE
    )
  }

  # Build the discrete plotting order once so every layer uses the same group
  # positions. The first key keeps BLC rows below TTP rows after coord_flip();
  # the second key orders groups within each scoring approach by their earliest
  # plotted window start.
  group_axis_levels <- data_peak_detection_window |>
    dplyr::group_by(.data[[grouping_variable]]) |>
    dplyr::summarise(
      scr_scoring_approach_order = min(
        as.integer(.data$scr_scoring_approach),
        na.rm = TRUE
      ),
      start_order = min(.data$start, na.rm = TRUE),
      end_order = max(.data$end, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data$scr_scoring_approach_order,
      .data$start_order,
      .data$end_order,
      .data[[grouping_variable]]
    ) |>
    dplyr::pull(dplyr::all_of(grouping_variable)) |>
    as.character()

  data_peak_detection_window <- data_peak_detection_window |>
    dplyr::mutate(
      plot_group = factor(
        as.character(.data[[grouping_variable]]),
        levels = group_axis_levels
      )
    )

  # Extend the lower limit by one second so the scoring-approach labels can sit
  # inside the panel without overlapping the first plotted interval.
  y_limits <- range(
    c(data_peak_detection_window$start - 1, data_peak_detection_window$end),
    na.rm = TRUE
  )

  if (any(!is.finite(y_limits))) {
    stop(
      "SCR window start and end values must contain finite numeric data.",
      call. = FALSE
    )
  }

  y_breaks <- seq(
    floor(min(data_peak_detection_window$start, na.rm = TRUE)),
    ceiling(max(data_peak_detection_window$end, na.rm = TRUE)),
    by = 1
  )

  n_groups <- nlevels(droplevels(data_peak_detection_window[[grouping_variable]]))

  cs_onset_label_x <- n_groups - max(1, round(n_groups * 0.1))

  graph <- data_peak_detection_window |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$plot_group
      )
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = .data$start,
        yend = .data$end,
        color = .data$window,
        group = .data$window
      ),
      linewidth = 7
    ) +
    ggplot2::labs(
      x = "Study",
      y = "Time (s) relative to stimulus onset",
      color = "Window:"
    ) +
    ggplot2::coord_flip(ylim = y_limits) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = min(y_limits),
        label = .data$scr_scoring_approach
      ),
      color = "black",
      hjust = 0,
      size = 3,
      fontface=2
    ) +
    ggplot2::geom_hline(yintercept = 0) + 
    ggplot2::geom_text(
      x = cs_onset_label_x,
      y = 0,
      angle = 90,
      label = "CS Onset",
      color = "black",
      vjust = -0.5,
      size = 5
    ) +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    theme(legend.position = "top")

  return(graph)
}
