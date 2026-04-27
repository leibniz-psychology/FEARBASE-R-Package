library(dplyr)
library(readr)
library(tidyr)

args = commandArgs(trailingOnly = TRUE)
root_path <- args[1] # root_path <- "."
grouping_variables <- c(
  "condition_id",
  "study_id",
  "paper_cond_id",
  "paper_study_id"
)

updateMapping <- function(path) {
  url <- 'https://docs.google.com/spreadsheets/d/1INi9MHloIm8XtaNLOoj046xf-T1Afm3vqFI-zRGKONw/edit?gid=0#gid=0'
  mapping <- read.csv(text = gsheet::gsheet2text(url, format = 'csv'))
  save(mapping, file = file.path(path, "data-preperation", "mapping.rda"))
  return(mapping)
}

csv_to_internal <- function(path) {
  mapping <- updateMapping(path)

  # read csv and give it the file's name
  files = list.files(
    file.path(path, "data-preperation", "input"),
    pattern = ".csv$"
  )
  for (f in files) {
    assign(
      sub(".csv", "", f),
      read_csv(file.path(path, "data-preperation", "input", f)),
      envir = .GlobalEnv
    )
  }

  data_long <<- data_long |>
    left_join(mapping, by = c("study_id" = "condition_id")) |>
    rename("condition_id" = "study_id", "study_id" = "study_id.y")
  save(data_long, file = file.path(path, "data", "data_long.rda"))
  save(data_wide, file = file.path(path, "data", "data_wide.rda"))
  save(codebook, file = file.path(path, "data", "codebook.rda"))
  save(
    questionnaires,
    file = file.path(path, "data", "questionnaires.rda")
  )

  metadata <<- metadata |>
    left_join(mapping, by = c("id" = "condition_id")) |>
    rename("condition_id" = "id")
  save(metadata, file = file.path(path, "data", "metadata.rda"))
  study_design <<- study_design |>
    left_join(mapping, by = c("study_id" = "condition_id")) |>
    rename("condition_id" = "study_id", "study_id" = "study_id.y")
  save(
    study_design,
    file = file.path(path, "data", "study_design.rda")
  )

  #   data_long[data_long$study_id == "98" & data_long$phase == "hab", ]
  #   data_long |> filter(study_id == "98", phase == "hab", measure == "scr")
}


prepareAgeData <- function(dl, path) {
  data_age <- dl |>
    filter(measure == "age") |>
    select(all_of(grouping_variables), participant_id, value, measure) |>
    mutate(
      age = as.numeric(value),
      across(all_of(grouping_variables), as.factor)
    ) |>
    filter(!is.na(age))
  save(
    data_age,
    file = file.path(path, "data", "data_age.rda")
  )
  return(data_age)
}
prepareInstructionsData <- function(md, path) {
  data_instructions <- md |>
    select(condition_id, study_id, starts_with("instruction")) |>
    group_by(study_id) |>
    distinct(instruction_contingency) |>
    group_by(instruction_contingency) |>
    summarise(n = n()) |>
    arrange(n) |>
    mutate(
      instruction_contingency = factor(
        instruction_contingency,
        levels = instruction_contingency
      )
    )
  save(
    data_instructions,
    file = file.path(path, "data", "data_instructions.rda")
  )
  return(data_instructions)
}

prepareSexData <- function(dl, path) {
  data_sex <- dl |>
    select(study_id, participant_id, value, measure) |>
    filter(measure == "sex" | measure == "gender")

  data_sex <- dl |>
    filter(!(participant_id %in% data_sex$participant_id)) |>
    select(study_id, participant_id) |>
    distinct() |>
    mutate(value = "not reported", measure = "sex") |>
    bind_rows(data_sex)

  data_sex <- data_sex |>
    mutate(
      sex = factor(
        stringr::str_split_i(tolower(value), "", 1),
        levels = c("m", "f", "n"),
        labels = c("m", "f", "nr") # c("male", "female", "not reported")
      )
    )
  save(
    data_sex,
    file = file.path(path, "data", "data_sex.rda")
  )
  return(data_sex)
}

prepareTrialCountData <- function(
  dl,
  path
) {
  data_trial_count <- dl |>
    select(
      all_of(grouping_variables),
      participant_id,
      phase,
      stimulus,
      trial
    ) |>
    drop_na(phase, trial) |>
    filter(phase != "int", phase != "other") |>
    distinct() |>
    group_by(
      across(all_of(grouping_variables)),
      participant_id,
      phase,
      stimulus
    ) |>
    summarise(trials = max(trial)) |>
    group_by(across(all_of(grouping_variables)), phase, stimulus, trials) |>
    summarise(n = n()) |>
    group_by(across(all_of(grouping_variables)), phase) |>
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
  save(
    data_trial_count,
    file = file.path(path, "data", "data_trial_count.rda")
  )
  return(data_trial_count)
}

preparePeakDetectionWindowData <- function(md, path) {
  data_peak_detection_window <- md |>
    select(
      paper_study_id,
      study_id,
      physiological_measure_scr_scoring_approach,
      physiological_measure_scr_baseline_window_start,
      physiological_measure_scr_baseline_window_end,
      physiological_measure_scr_peak_detection_window_min,
      physiological_measure_scr_peak_detection_window_max
    ) |>
    drop_na(physiological_measure_scr_peak_detection_window_max) |>
    distinct() |>
    arrange(
      physiological_measure_scr_scoring_approach,
      desc(physiological_measure_scr_peak_detection_window_min),
      desc(physiological_measure_scr_peak_detection_window_max)
    ) |>
    mutate(across(any_of(grouping_variables), as.factor)) |>
    pivot_longer(
      cols = -c(
        any_of(grouping_variables),
        physiological_measure_scr_scoring_approach
      ),
      names_to = c("measure", "window", "timepoint"),
      names_pattern = "(scr)_(.*)_window_(.*)"
    ) |>
    mutate(
      timepoint = forcats::fct_recode(timepoint, start = "min", end = "max")
    ) |>
    pivot_wider(
      names_from = timepoint,
      values_from = value
    ) |>
    mutate(
      physiological_measure_scr_scoring_approach = forcats::fct_recode(
        physiological_measure_scr_scoring_approach,
        "BLC" = "baseline_correction",
        "TTP" = "trough-to-peak"
      ),
      window = case_when(
        physiological_measure_scr_scoring_approach !=
          "BLC" ~ "Trough Detection",
        physiological_measure_scr_scoring_approach == "BLC" &
          window == "peak_detection" ~ "Peak Detection",
        physiological_measure_scr_scoring_approach == "BLC" &
          window == "baseline" ~ "Baseline",
        TRUE ~ window
      ) |>
        factor(levels = c("Baseline", "Peak Detection", "Trough Detection"))
    )
  save(
    data_peak_detection_window,
    file = file.path(path, "data", "data_peak_detection_window.rda")
  )
  return(data_peak_detection_window)
}

prepareSampleSizeData <- function(dl, path) {
  data_sample_size <- dl |>
    select(paper_study_id, study_id, participant_id) |>
    unique() |>
    group_by(paper_study_id, study_id) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    mutate(across(any_of(grouping_variables), as.factor))

  save(
    data_sample_size,
    file = file.path(path, "data", "data_sample_size.rda")
  )
  return(data_sample_size)
}

prepareCollectionYearData <- function(md, path) {
  data_collection_year <- md |>
    select(year) |>
    table() |>
    as_tibble() |>
    mutate(year = as.numeric(year))
  save(
    data_collection_year,
    file = file.path(path, "data", "data_collection_year.rda")
  )
  return(data_collection_year)
}

prepareRRData <- function(md, path) {
  data_reinforcement_rate <- md |>
    select(study_id, starts_with("reinf")) |>
    pivot_longer(cols = -study_id, names_to = "Reinforcement Rate") |>
    drop_na(value) |>
    mutate(value = floor(value)) |>
    group_by(value) |>
    summarise(n = n())
  save(
    data_reinforcement_rate,
    file = file.path(path, "data", "data_reinforcement_rate.rda")
  )
  return(data_reinforcement_rate)
}

reorderPhases <- function(phases) {
  unique_phases <- unique(as.character(phases))
  priority_phases <- c("hab", "acq", "ext", "int", "rin", "rex", "rev", "other")
  existing_priority <- priority_phases[priority_phases %in% unique_phases]
  other_phases <- setdiff(unique_phases, priority_phases)
  phase_levels <- c(existing_priority, other_phases)

  return(factor(phases, levels = phase_levels))
}

.get_co_occurrence_data <- function(df, cat_var, id_var, count_var) {
  x_wide <- df |>
    select(all_of(c(cat_var, id_var, count_var))) |>
    pivot_wider(
      names_from = all_of(id_var),
      values_from = all_of(count_var),
      values_fill = 0
    )

  categories <- x_wide[[cat_var]]
  x_mat <- x_wide |> select(-all_of(cat_var)) |> as.matrix()

  crosstable_mat <- x_mat %*% t(x_mat > 0)

  crosstable <- as.data.frame(crosstable_mat)
  colnames(crosstable) <- categories
  crosstable[[cat_var]] <- categories

  crosstable_long <- crosstable |>
    pivot_longer(
      cols = -all_of(cat_var),
      names_to = paste0(cat_var, "2"),
      values_to = "value"
    )

  return(crosstable_long)
}


preparePhasesHeatmapData <- function(dl, path) {
  # Prepare phase data
  phase_data <- dl |>
    select(condition_id, participant_id, phase) |>
    distinct() |>
    drop_na(phase) |>
    filter(phase != "int", phase != "other") |>
    group_by(condition_id, phase) |>
    summarise(used = n(), .groups = "drop")

  # Summary for bar plot
  data_phases_barplot <- phase_data |>
    group_by(phase) |>
    summarise(used = sum(used), .groups = "drop")

  data_phases_barplot$phase <- forcats::fct_rev(reorderPhases(
    data_phases_barplot$phase
  ))

  # Compute co-occurrence
  data_phases_heatmap <- .get_co_occurrence_data(
    phase_data,
    "phase",
    "condition_id",
    "used"
  )

  # Apply the same levels to the heatmap data
  data_phases_heatmap$phase <- reorderPhases(data_phases_heatmap$phase) |>
    forcats::fct_recode(
      Hab = "hab",
      Acq = "acq",
      Ext = "ext",
      RI = "rin",
      `Re-Ext` = "rex",
      Rev = "rev"
    )
  data_phases_heatmap$phase2 <- forcats::fct_rev(reorderPhases(
    data_phases_heatmap$phase2
  )) |>
    forcats::fct_recode(
      Hab = "hab",
      Acq = "acq",
      Ext = "ext",
      RI = "rin",
      `Re-Ext` = "rex",
      Rev = "rev"
    )

  data_phases <- list(
    heatmap_data = data_phases_heatmap,
    barplot_data = data_phases_barplot
  )
  save(
    data_phases,
    file = file.path(path, "data", "data_phases.rda")
  )
  return(data_phases)
}

prepareMeasuresHeatmapData <- function(dl, md, path) {
  fulld <- dl |>
    left_join(md, by = "condition_id")

  # Define categories
  quest_measures <- c(
    "stais",
    "stait",
    "neo-ffi",
    "asi",
    "promis",
    "ius",
    "pswq"
  )
  phys_measures <- c("scr", "scl", "ps", "fps", "hr")
  rate_measures <- c("anx", "arous", "aware", "expect", "fear", "val", "stress")

  # Categorize and deduplicate
  measure_data <- fulld |>
    filter(
      measure %in% c(quest_measures, phys_measures, rate_measures)
    ) |>
    mutate(
      type = case_when(
        measure %in% quest_measures ~ "questionnaire",
        measure %in% phys_measures ~ "physiological",
        measure %in% rate_measures ~ "rating"
      )
    ) |>
    mutate(
      measure = forcats::fct_recode(
        measure,
        SCR = "scr",
        FPS = "fps",
        PD = "ps",
        Expectancy = "expect",
        Fear = "fear",
        Awareness = "aware",
        Arousal = "arous",
        Valence = "val",
        `STAI-T` = "stait",
        IUS = "ius",
        `STAI-S` = "stais",
        ASI = "asi"
      )
    ) |>
    mutate(
      type = factor(
        type,
        levels = c("physiological", "rating", "questionnaire")
      )
    ) |>
    select(condition_id, participant_id, measure, type) |>
    distinct()

  # Prepare summary for bar plot
  measure_cat <- measure_data |>
    group_by(measure, type) |>
    summarise(used = n(), .groups = "drop") |>
    arrange(type, desc(used))

  # Set the order of measures based on the arranged summary
  measure_levels <- measure_cat$measure
  measure_cat$measure <- factor(
    measure_cat$measure,
    levels = rev(measure_levels)
  )

  # Compute co-occurrence
  co_data <- measure_data |>
    group_by(condition_id, measure) |>
    summarise(n = n(), .groups = "drop")

  crosstable_long <- .get_co_occurrence_data(
    co_data,
    "measure",
    "condition_id",
    "n"
  )

  # Apply the same levels to the heatmap data
  # x-axis from left to right, y-axis from top to bottom
  crosstable_long$measure <- factor(
    crosstable_long$measure,
    levels = measure_levels
  )
  crosstable_long$measure2 <- factor(
    crosstable_long$measure2,
    levels = rev(measure_levels)
  )

  data_measures <- list(
    heatmap_data = crosstable_long,
    barplot_data = measure_cat
  )
  save(
    data_measures,
    file = file.path(path, "data", "data_measures.rda")
  )
  return(data_measures)
}

csv_to_internal(root_path)
prepareAgeData(data_long, root_path)
prepareInstructionsData(metadata, root_path)
prepareSexData(data_long, root_path)
prepareTrialCountData(data_long, root_path)
preparePeakDetectionWindowData(metadata, root_path)
prepareSampleSizeData(data_long, root_path)
prepareCollectionYearData(metadata, root_path)
prepareRRData(metadata, root_path)
preparePhasesHeatmapData(data_long, root_path)
prepareMeasuresHeatmapData(data_long, metadata, root_path)
