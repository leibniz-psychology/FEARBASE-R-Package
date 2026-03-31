#' @title Phase Length
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

trialsPhaseParticipant <- function(y_axis = "n", alt = FALSE) {
  dl <- getDataLong()

  age <- dl |>
    filter(measure == "age") |>
    select(condition_id, participant_id, value, measure) |>
    mutate(age = as.numeric(value), condition_id = as.factor(condition_id)) |>
    filter(!is.na(age))

  study_order <- age |>
    group_by(condition_id) |>
    summarise(mean_age = median(age)) |>
    arrange(desc(mean_age)) |>
    pull(condition_id)

  trials <- .prepareTrialCountData(dl, alt = alt)
  # trials$condition_id <- factor(trials$condition_id, levels = study_order)

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

.prepareTrialCountData <- function(dl, alt = FALSE) {
  if (alt) {
    dat <- dl |>
      select(condition_id, participant_id, phase, stimulus, trial) |>
      drop_na(phase, trial) |>
      filter(phase != "int") |>
      distinct() |>
      group_by(condition_id, participant_id, phase) |>
      summarise(trials = n()) |>
      group_by(condition_id, phase, trials) |>
      summarise(n = n()) |>
      ungroup() |>
      mutate(
        condition_id = as.factor(condition_id),
        phase = reorderPhases(phase)
      )
  } else {
    dat <- dl |>
      select(condition_id, participant_id, phase, stimulus, trial) |>
      drop_na(phase, trial) |>
      filter(phase != "int") |>
      distinct() |>
      group_by(condition_id, participant_id, phase) |>
      summarise(trials = max(trial)) |>
      group_by(condition_id, phase, trials) |>
      summarise(n = n()) |>
      ungroup() |>
      mutate(
        condition_id = as.factor(condition_id),
        phase = reorderPhases(phase)
      )
  }
  return(dat)
}

trialsPhaseParticipantDescriptive <- function() {
  dl <- getDataLong()

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

studyDesign <- function(sd) {
  # in "study_design" the "condition_id" is still named "study_id" and "phase" is named "name"

  trials <- sd |>
    drop_na(cspTrials) |>
    filter(name != "int") |>
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
