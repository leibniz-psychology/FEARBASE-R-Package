#' @title exploration
#' @import dplyr
#' @import ggplot2
#' @description
#' generate an overview of the full dataset
#' @export

peakDetectionWindows <- function() {
  metadata <- getMetadata()

  graph <- metadata |>
    select(
      condition_id,
      scr_baseline_window_start,
      scr_baseline_window_end,
      scr_peak_detection_window_min,
      scr_peak_detection_window_max
    ) |>
    drop_na(scr_peak_detection_window_max) |>
    distinct() |>
    arrange(desc(scr_peak_detection_window_max)) |>
    mutate(condition_id = factor(condition_id, levels = condition_id)) |>
    pivot_longer(
      cols = -c(condition_id),
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
    ggplot(aes(x = condition_id, color = window, group = window)) +
    geom_segment(
      aes(
        y = start,
        yend = end
      ),
      linewidth = 7
    ) +
    labs(
      title = "SCR Detection Windows by Study",
      x = "Study",
      y = "Time (s)",
      fill = "Window"
    ) +
    coord_flip()

  return(graph)
}
