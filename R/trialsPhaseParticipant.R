#' @title sex distribution
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

trialsPhaseParticipant <- function(y = "n") {
  dl <- getDataLong()

  trials <- dl |>
    select(condition_id, participant_id, phase, stimulus, trial) |>
    drop_na() |>
    distinct() |>
    filter(condition_id != "98") |>
    group_by(condition_id, participant_id, phase) |>
    summarise(trials = max(trial)) |>
    group_by(condition_id, phase, trials) |>
    summarise(n = n()) |>
    ungroup() |>
    mutate(
      condition_id = as.factor(condition_id),
      phase = reorderPhases(phase)
    )

  if (tolower(y) %in% c("n", "participant", "participants")) {
    graph <- trials |>
      ggplot(aes(
        x = trials,
        y = n,
        fill = condition_id,
        group = condition_id
      )) +
      geom_bar(stat = "identity", color = "white", linewidth = .2) +
      facet_grid(rows = vars(phase), axes = "all", axis.labels = "all_x") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(
        x = "Number of Trials",
        y = "Number of Participants",
        fill = "Condition ID"
      )
  } else if (tolower(y) %in% c("study", "studies", "s")) {
    graph <- trials |>
      group_by(condition_id, phase, trials) |>
      summarise(n = n()) |>
      ggplot(aes(x = trials, y = n)) +
      geom_bar(stat = "identity") +
      facet_grid(rows = vars(phase), axes = "all", axis.labels = "all_x") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(x = "Number of Trials", y = "Number of Studies")
  }

  return(graph)
}
