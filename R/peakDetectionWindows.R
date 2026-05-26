#' Peak detection windows
#'
#' @description
#' Generates a graph of the SCR Scoring Windows
#'
#' @param md The metadata.
#' @param grouping_variable A string specifying the variable to group by (allowed
#'   values: "study_id" or "paper_study_id").
#'
#' @return A ggplot object.
#' @export
peakDetectionWindows <- function(
  md,
  grouping_variable = "study_id"
) {
  md <- .apply_mapping_to_metadata(md)

  available_grouping_variables <- intersect(
    c("study_id", "paper_study_id"),
    names(md)
  )

  if (length(available_grouping_variables) == 0) {
    stop("No supported grouping variable is available in the metadata.")
  }

  if (!(grouping_variable %in% available_grouping_variables)) {
    if (grouping_variable == "paper_study_id" && "study_id" %in% names(md)) {
      grouping_variable <- "study_id"
    } else {
      stop(
        "grouping_variable must be one of: ",
        paste(available_grouping_variables, collapse = ", ")
      )
    }
  }

  if (all(c(
    "physio_scr_scoring_approach",
    "physio_scr_baseline_window_start",
    "physio_scr_baseline_window_end",
    "physio_scr_peak_detection_window_min",
    "physio_scr_peak_detection_window_max"
  ) %in% names(md))) {
    scr_scoring_col <- "physio_scr_scoring_approach"
    scr_baseline_start_col <- "physio_scr_baseline_window_start"
    scr_baseline_end_col <- "physio_scr_baseline_window_end"
    scr_peak_min_col <- "physio_scr_peak_detection_window_min"
    scr_peak_max_col <- "physio_scr_peak_detection_window_max"
  } else if (all(c(
    "scr_scoring_approach",
    "scr_baseline_window_start",
    "scr_baseline_window_end",
    "scr_peak_detection_window_min",
    "scr_peak_detection_window_max"
  ) %in% names(md))) {
    scr_scoring_col <- "scr_scoring_approach"
    scr_baseline_start_col <- "scr_baseline_window_start"
    scr_baseline_end_col <- "scr_baseline_window_end"
    scr_peak_min_col <- "scr_peak_detection_window_min"
    scr_peak_max_col <- "scr_peak_detection_window_max"
  } else {
    stop(
      "The metadata does not contain a supported set of SCR window columns."
    )
  }

  data_peak_detection_window <- md |>
    select(
      any_of(c("paper_study_id", "study_id")),
      all_of(c(
        scr_scoring_col,
        scr_baseline_start_col,
        scr_baseline_end_col,
        scr_peak_min_col,
        scr_peak_max_col
      ))
    ) |>
    rename(
      scr_scoring_approach = all_of(scr_scoring_col),
      scr_baseline_window_start = all_of(scr_baseline_start_col),
      scr_baseline_window_end = all_of(scr_baseline_end_col),
      scr_peak_detection_window_min = all_of(scr_peak_min_col),
      scr_peak_detection_window_max = all_of(scr_peak_max_col)
    ) |>
    drop_na(scr_peak_detection_window_max) |>
    distinct() |>
    arrange(
      scr_scoring_approach,
      desc(scr_peak_detection_window_min),
      desc(scr_peak_detection_window_max)
    ) |>
    mutate(across(any_of(c("paper_study_id", "study_id")), as.factor)) |>
    pivot_longer(
      cols = -c(
        any_of(c("paper_study_id", "study_id")),
        scr_scoring_approach
      ),
      names_to = c("measure", "window", "timepoint"),
      names_pattern = "(scr)_(.*)_window_(.*)"
    ) |>
    mutate(
      timepoint = forcats::fct_recode(timepoint, start = "min", end = "max")
    ) |>
    pivot_wider(
      names_from = timepoint,
      values_from = value
    ) |>
    mutate(
      scr_scoring_approach = forcats::fct_recode(
        scr_scoring_approach,
        "BLC" = "baseline_correction",
        "TTP" = "trough-to-peak"
      ),
      window = case_when(
        scr_scoring_approach != "BLC" ~ "Trough Detection",
        scr_scoring_approach == "BLC" &
          window == "peak_detection" ~ "Peak Detection",
        scr_scoring_approach == "BLC" &
          window == "baseline" ~ "Baseline",
        TRUE ~ window
      ) |>
        factor(levels = c("Baseline", "Peak Detection", "Trough Detection"))
    )

  # Plot
  y_limits <- range(
    c(data_peak_detection_window$start, data_peak_detection_window$end, -3, 0),
    na.rm = TRUE
  )

  graph <- data_peak_detection_window |>
    ggplot(aes(
      x = forcats::fct_reorder(
        .data[[grouping_variable]],
        as.numeric(as.factor(scr_scoring_approach))
      )
    )) +
    geom_segment(
      aes(
        y = start,
        yend = end,
        color = window,
        group = window
      ),
      linewidth = 7
    ) +
    labs(
      x = "Study",
      y = "Time (s) relative to stimulus onset",
      color = "Detection Window"
    ) +
    coord_flip(ylim = y_limits) +
    geom_text(
      aes(y = -3, label = scr_scoring_approach),
      color = "black",
      hjust = 0,
      size = 3
    ) +
    geom_hline(yintercept = 0) +
    geom_text(
      x = 16,
      y = 0,
      angle = 90,
      group = "none",
      label = "CS Onset",
      color = "black",
      vjust = -0.1,
      size = 5
    )

  return(graph)
}
