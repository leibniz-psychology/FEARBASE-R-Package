#' Phase length
#'
#' @description
#' This function calculates the number of trials per phase for n participants and s studies,
#' and visualizes the distribution of trial counts across phases.
#'
#' @param dl A data frame in long format containing the trial data.
#' @param level A string specifying the y-axis variable: "n" for number of participants, "s" for number of studies.
#'
#' @return A ggplot object visualizing the distribution of trial counts across phases.
#' @export
trialsPhaseParticipant <- function(
  dl,
  level = "n_studies"
) {
  # Process Data
  data_trial_count <- dl |>
      select(
        study_id,
        participant_id,
        phase,
        stimulus,
        trial
      ) |>
      drop_na(phase, trial) |>
      filter(phase != "int", phase != "other") |>
      distinct() |>
      group_by(
        across(study_id),
        participant_id,
        phase,
        stimulus
      ) |>
      summarise(trials = max(trial)) |>
      group_by(across(study_id), phase, stimulus, trials) |>
      summarise(n = n()) |>
      group_by(across(study_id), phase) |>
      summarise(trials = sum(trials), n = unique(n)) |>
      ungroup() |>
      mutate(
        study_id = as.factor(study_id),
        phase = reorderPhases(phase) |>
          forcats::fct_recode(
            Hab = "hab",
            Acq = "acq",
            Ext = "ext",
            RI = "rin",
            `Re-Ext` = "rex",
            Rev = "rev"
          )
      )

  # Plot
  if (tolower(level) %in% c("n_subjects", "participant", "participants")) {
    graph <- data_trial_count |>
      ggplot(aes(
        x = trials,
        y = n,
        fill = .data[['study_id']],
        group = .data[['study_id']]
      )) +
      geom_bar(stat = "identity", color = "white", linewidth = .2) +
      facet_grid(rows = vars(phase), axes = "all", axis.labels = "all_x") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(
        x = "Number of Trials",
        y = "Number of Participants",
        fill = "Study ID"
      )
  } else if (tolower(level) %in% c("study", "studies", "n_studies")) {
    graph <- data_trial_count |>
      group_by(.data[['study_id']], phase, trials) |>
      summarise(n = n()) |>
      ggplot(aes(x = trials, y = n)) +
      geom_bar(stat = "identity") +
      facet_grid(rows = vars(phase), axes = "all", axis.labels = "all_x") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(x = "Number of Trials", y = "Number of Studies")
  }
  return(graph)
}


trialsPhaseParticipantDescriptive <- function(dl) {
  trials <- dl |>
    select(
      .data[['study_id']],
      participant_id,
      phase,
      stimulus,
      trial
    ) |>
    drop_na() |>
    distinct() |>
    group_by(.data[['study_id']], participant_id, phase) |>
    summarise(trials = max(trial)) |>
    ungroup() |>
    mutate(
      !!study_id := as.factor(.data[['study_id']]),
      phase = reorderPhases(phase)
    )
  psych::describeBy(trials, group = "phase")
}

studyDesign <- function(sd) {
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
