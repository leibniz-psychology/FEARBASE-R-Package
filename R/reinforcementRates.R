#' Reinforcement rates
#'
#' @description
#' Creates a bar graph of the studies' reinforcement rates.
#'
#' @param md The metadata.
#'
#' @return A ggplot object.
#' @export
reinforcementRates <- function(md) {
  # Process data
  data_reinforcement_rate <- md |>
    select(id, starts_with("reinf")) |>
    pivot_longer(cols = -id, names_to = "Reinforcement Rate") |>
    drop_na(value) |>
    mutate(value = floor(value)) |>
    group_by(value) |>
    summarise(n = n()
  )

  # Plot
  graph <- data_reinforcement_rate |>
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

reinforcementRateDescriptives <- function(md = metadata) {
  md |>
    select(study_id, starts_with("reinf")) |>
    pivot_longer(cols = -study_id, names_to = "stimulus") |>
    pull(value) |>
    psych::describe()
}
