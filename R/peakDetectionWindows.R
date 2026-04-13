#' Peak detection windows
#'
#' @description
#' Generates a graph of the SCR Scoring Windows
#'
#' @param md The metadata.
#'
#' @return A ggplot object.
#' @export
peakDetectionWindows <- function(md = metadata) {
  graph <- metadata |>
    select(
      paper_study_id,
      scr_scoring_approach,
      scr_baseline_window_start,
      scr_baseline_window_end,
      scr_peak_detection_window_min,
      scr_peak_detection_window_max
    ) |>
    drop_na(scr_peak_detection_window_max) |>
    distinct() |>
    arrange(
      scr_scoring_approach,
      desc(scr_peak_detection_window_min),
      desc(scr_peak_detection_window_max)
    ) |>
    mutate(paper_study_id = factor(paper_study_id, levels = paper_study_id)) |>
    pivot_longer(
      cols = -c(paper_study_id, scr_scoring_approach),
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
        scr_scoring_approach == "BLC" & window == "baseline" ~ "Baseline",
        TRUE ~ window
      ) |>
        factor(levels = c("Baseline", "Peak Detection", "Trough Detection"))
    ) |>
    ggplot(aes(x = paper_study_id, color = window, group = window)) +
    geom_segment(
      aes(
        y = start,
        yend = end
      ),
      linewidth = 7
    ) +
    labs(
      x = "Study",
      y = "Time (s)",
      color = "Detection Window"
    ) +
    coord_flip(ylim = c(-5, 8)) + # TODO: set limits dynamically
    geom_text(
      aes(y = -3, label = scr_scoring_approach),
      color = "black",
      hjust = 1,
      size = 3
    ) +
    geom_hline(yintercept = 0)

  return(graph)
}
