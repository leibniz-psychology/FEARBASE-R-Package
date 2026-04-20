#' Peak detection windows
#'
#' @description
#' Generates a graph of the SCR Scoring Windows
#'
#' @param md The metadata.
#'
#' @return A ggplot object.
#' @export
peakDetectionWindows <- function(dat = data_peak_detection_window) {
  graph <- dat |>
    ggplot(aes(x = paper_study_id)) +
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
