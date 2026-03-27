#' @title exploration
#' @import dplyr
#' @import ggplot2
#' @description
#' generate an overview of the full dataset
#' @export

reinforcementRates <- function() {
  metadata <- getMetadata()

  data <- metadata |>
    select(study_id, starts_with("reinf")) |>
    pivot_longer(cols = -study_id, names_to = "Reinforcement Rate") |>
    drop_na(value) |>
    mutate(value = floor(value)) |>
    group_by(value) |>
    summarise(n = n())
  graph <- data |>
    ggplot(aes(x = value, y = n)) +
    geom_bar(stat = "identity", color = "white") +
    scale_x_continuous(
      breaks = seq(30, 100, 10)
    ) +
    labs(
      x = "Reinforcement Rate",
      y = "Studies"
    )

  return(graph)
}

reinforcementRateDescriptives <- function() {
  metadata <- getMetadata()

  metadata |>
    select(study_id, starts_with("reinf")) |>
    pivot_longer(cols = -study_id, names_to = "stimulus") |>
    pull(value) |>
    psych::describe()
}
