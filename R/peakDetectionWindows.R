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
      scr_peak_detection_window_min,
      scr_peak_detection_window_max
    ) |>
    drop_na() |>
    distinct() |>
    mutate(
      diff = scr_peak_detection_window_max - scr_peak_detection_window_min
    ) |>
    arrange(desc(diff), desc(scr_peak_detection_window_max)) |>
    mutate(condition_id = factor(condition_id, levels = condition_id)) |>
    pivot_longer(cols = -c(condition_id, diff), names_to = "window") |>
    ggplot(aes(x = condition_id, y = value, fill = window, group = window)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(
      title = "SCR Peak Detection Windows by Study",
      x = "Study",
      y = "Time (s)",
      fill = "Window"
    )

  return(graph)
}
