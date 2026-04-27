#' Phase length
#'
#' @description
#' This function calculates the number of trials per phase for n participants and s studies,
#' and visualizes the distribution of trial counts across phases.
#'
#' @param dl A data frame in long format containing the trial data.
#' @param y_axis A string specifying the y-axis variable: "n" for number of participants, "s" for number of studies.
#' @param grouping_variable A string specifying the variable to group by (allowed values: "condition_id", "study_id", "paper_cond_id", or "paper_study_id").
#'
#' @return A ggplot object visualizing the distribution of trial counts across phases.
#' @export
trialsPhaseParticipant <- function(
  dat = data_trial_count,
  y_axis = "s",
  grouping_variable = "condition_id"
) {
  if (tolower(y_axis) %in% c("n", "participant", "participants")) {
    graph <- dat |>
      ggplot(aes(
        x = trials,
        y = n,
        fill = .data[[grouping_variable]],
        group = .data[[grouping_variable]]
      )) +
      geom_bar(stat = "identity", color = "white", linewidth = .2) +
      facet_grid(rows = vars(phase), axes = "all", axis.labels = "all_x") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(
        x = "Number of Trials",
        y = "Number of Participants",
        fill = "Study ID"
      )
  } else if (tolower(y_axis) %in% c("study", "studies", "s")) {
    graph <- dat |>
      group_by(.data[[grouping_variable]], phase, trials) |>
      summarise(n = n()) |>
      ggplot(aes(x = trials, y = n)) +
      geom_bar(stat = "identity") +
      facet_grid(rows = vars(phase), axes = "all", axis.labels = "all_x") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(x = "Number of Trials", y = "Number of Studies")
  }
  return(graph)
}


trialsPhaseParticipantDescriptive <- function(
  dl = data_long,
  grouping_variable = "condition_id"
) {
  trials <- dl |>
    select(
      .data[[grouping_variable]],
      participant_id,
      phase,
      stimulus,
      trial
    ) |>
    drop_na() |>
    distinct() |>
    group_by(.data[[grouping_variable]], participant_id, phase) |>
    summarise(trials = max(trial)) |>
    ungroup() |>
    mutate(
      !!grouping_variable := as.factor(.data[[grouping_variable]]),
      phase = reorderPhases(phase)
    )
  psych::describeBy(trials, group = "phase")
}

studyDesign <- function(sd = study_design) {
  # in "study_design", "phase" is named "name"

  trials <- sd |>
    drop_na(cspTrials, csmTrials) |>
    filter(name != "int", name != "other") |>
    distinct() |>
    group_by(study_id, name) |>
    summarise(trials = sum(cspTrials)) |>
    ungroup() |>
    mutate(
      study_id = as.factor(study_id),
      name = reorderPhases(name)
    )

  graph <- trials |>
    group_by(study_id, name, trials) |>
    summarise(n = n()) |>
    ggplot(aes(x = trials, y = n)) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(name), axes = "all", axis.labels = "all_x") +
    scale_x_continuous(breaks = scales::extended_breaks(10)) +
    labs(x = "Number of Trials", y = "Number of Studies")

  return(graph)
}
