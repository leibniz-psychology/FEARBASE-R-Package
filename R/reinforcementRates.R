#' @title exploration
#' @import dplyr
#' @import ggplot2
#' @description
#' generate an overview of the full dataset
#' @export

reinforcementRates <- function() {
  metadata <- getMetadata()

  graph <- metadata |>
    select(study_id, starts_with("reinf")) |>
    pivot_longer(cols = -study_id, names_to = "stimulus") |>
    drop_na(value) |>
    ggplot(aes(x = value)) +
    geom_histogram(bins = 6, color = "white") +
    labs(
      x = "Reinforcement Rate",
      y = "Studies"
    ) +
    scale_x_continuous(breaks = seq(30, 100, by = 10))

  return(graph)
}
