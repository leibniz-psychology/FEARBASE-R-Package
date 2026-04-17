#' Phase length
#'
#' @description
#' This function calculates the number of trials per phase for n participants and s studies,
#' and visualizes the distribution of trial counts across phases.
#'
#' @param dl A data frame in long format containing the trial data.
#' @param y_axis A string specifying the y-axis variable: "n" for number of participants, "s" for number of studies.
#'
#' @return A ggplot object visualizing the distribution of trial counts across phases.
#' @export
trialsPhaseParticipant <- function(dl = data_long, y_axis = "s") {
  trials <- .prepareTrialCountData(dl = dl)

  if (tolower(y_axis) %in% c("n", "participant", "participants")) {
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
        fill = "Study ID"
      )
  } else if (tolower(y_axis) %in% c("study", "studies", "s")) {
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

.prepareTrialCountData <- function(
  dl = data_long,
  # sd = study_design,
  from_study_design = FALSE
) {
  dat <- dl |>
    select(condition_id, participant_id, phase, stimulus, trial) |>
    drop_na(phase, trial) |>
    filter(phase != "int", phase != "other") |>
    distinct() |>
    group_by(condition_id, participant_id, stimulus, phase) |>
    summarise(trials = max(trial)) |>
    group_by(condition_id, phase, stimulus, trials) |>
    summarise(n = n()) |>
    group_by(condition_id, phase) |>
    summarise(trials = sum(trials), n = unique(n)) |>
    ungroup() |>
    mutate(
      condition_id = as.factor(condition_id),
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
  return(dat)
}

trialsPhaseParticipantDescriptive <- function(dl = data_long) {
  trials <- dl |>
    select(condition_id, participant_id, phase, stimulus, trial) |>
    drop_na() |>
    distinct() |>
    group_by(condition_id, participant_id, phase) |>
    summarise(trials = max(trial)) |>
    ungroup() |>
    mutate(
      condition_id = as.factor(condition_id),
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
