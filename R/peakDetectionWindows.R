#' Peak detection windows
#'
#' @description
#' Generates a graph of the SCR Scoring Windows
#'
#' @param md The metadata.
#'
#' @return A ggplot object.
#' @export
peakDetectionWindows <- function(md) {
  data_peak_detection_window <- md |>
    select(
      id,
      physiological_measure_scr_scoring_approach,
      physiological_measure_scr_baseline_window_start,
      physiological_measure_scr_baseline_window_end,
      physiological_measure_scr_peak_detection_window_min,
      physiological_measure_scr_peak_detection_window_max
    ) |>
    drop_na(physiological_measure_scr_peak_detection_window_max) |>
    distinct() |>
    arrange(
      physiological_measure_scr_scoring_approach,
      desc(physiological_measure_scr_peak_detection_window_min),
      desc(physiological_measure_scr_peak_detection_window_max)
    ) |>
    mutate(across(id, as.factor)) |>
    pivot_longer(
      cols = -c(
        id,
        physiological_measure_scr_scoring_approach
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
      physiological_measure_scr_scoring_approach = forcats::fct_recode(
        physiological_measure_scr_scoring_approach,
        "BLC" = "baseline_correction",
        "TTP" = "trough-to-peak"
      ),
      window = case_when(
        physiological_measure_scr_scoring_approach !=
          "BLC" ~ "Trough Detection",
        physiological_measure_scr_scoring_approach == "BLC" &
          window == "peak_detection" ~ "Peak Detection",
        physiological_measure_scr_scoring_approach == "BLC" &
          window == "baseline" ~ "Baseline",
        TRUE ~ window
      ) |>
        factor(levels = c("Baseline", "Peak Detection", "Trough Detection"))
    )

  # Plot
  graph <- data_peak_detection_window |>
    ggplot(aes(
      x = forcats::fct_reorder(
        .data[['id']],
        as.numeric(as.factor(physiological_measure_scr_scoring_approach))
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
    coord_flip() + # TODO: set limits dynamically
    geom_text(
      aes(y = -3, label = physiological_measure_scr_scoring_approach),
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
