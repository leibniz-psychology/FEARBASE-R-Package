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
    ggplot(aes(x = value)) +
    geom_histogram() +
    labs(
      title = "Reinforcement Rates Histogram",
      x = "Reinforcement Rate",
      y = "Studies"
    )

  return(graph)
}
