fixture_codebook <- function() {
  tibble::tibble(
    attribute = c(
      rep("phase", 5L),
      rep("measure", 4L)
    ),
    abbreviation = c(
      "hab",
      "acq",
      "ext",
      "int",
      "other",
      "age",
      "sex",
      "scr",
      "us_exp"
    ),
    name = c(
      "habituation",
      "acquisition",
      "extinction",
      "intervention",
      "other",
      "age",
      "sex",
      "skin conductance response",
      "US expectancy"
    )
  )
}

fixture_long_data <- function() {
  tibble::tribble(
    ~condition_id, ~study_id, ~paper_cond_id, ~paper_study_id, ~participant_id, ~measure, ~value, ~phase, ~stimulus, ~trial,
    "c1", "s1", "pc1", "ps1", "p1", "age", "21", "hab", "cs+", 1,
    "c1", "s1", "pc1", "ps1", "p1", "sex", "m", "hab", "cs+", 1,
    "c1", "s1", "pc1", "ps1", "p1", "scr", "0.3", "hab", "cs+", 2,
    "c1", "s1", "pc1", "ps1", "p1", "scr", "0.4", "acq", "cs+", 1,
    "c1", "s1", "pc1", "ps1", "p1", "us_exp", "70", "acq", "cs-", 1,
    "c1", "s1", "pc1", "ps1", "p2", "age", "24", "hab", "cs+", 1,
    "c1", "s1", "pc1", "ps1", "p2", "sex", "f", "hab", "cs+", 1,
    "c1", "s1", "pc1", "ps1", "p2", "scr", "0.2", "ext", "cs+", 1,
    "c1", "s1", "pc1", "ps1", "p2", "us_exp", "30", "ext", "cs-", 1,
    "c2", "s2", "pc2", "ps2", "p3", "age", "29", "hab", "cs+", 1,
    "c2", "s2", "pc2", "ps2", "p3", "sex", NA_character_, "hab", "cs+", 1,
    "c2", "s2", "pc2", "ps2", "p3", "scr", "0.5", "acq", "cs+", 1,
    "c2", "s2", "pc2", "ps2", "p3", "scr", "0.6", "acq", "cs+", 2,
    "c2", "s2", "pc2", "ps2", "p4", "age", "31", "hab", "cs+", 1,
    "c2", "s2", "pc2", "ps2", "p4", "us_exp", "20", "ext", "cs-", 1
  )
}

fixture_metadata <- function() {
  tibble::tribble(
    ~condition_id, ~study_id, ~year, ~year_data, ~n_subjects, ~us_type, ~cs_type, ~instruction_contingency, ~reinf_acq, ~reinf_ext, ~physio_scr_scoring_approach, ~physio_scr_baseline_window_start, ~physio_scr_baseline_window_end, ~physio_scr_peak_detection_window_min, ~physio_scr_peak_detection_window_max,
    "c1", "s1", 2020, 2019, 2, "electrotactile", "visual", "Fully instructed (whole exp)", 100, 50, "baseline_correction", -1, 0, 1, 4,
    "c2", "s2", 2021, 2020, 2, "auditory", "visual", "Uninstructed (whole exp)", 75, 25, "trough-to-peak", -2, 0, 2, 5
  )
}

fixture_study_design <- function() {
  tibble::tribble(
    ~condition_id, ~study_id, ~name, ~cspTrials, ~csmTrials,
    "c1", "s1", "hab", 2, 2,
    "c1", "s1", "acq", 4, 4,
    "c2", "s2", "hab", 2, 2,
    "c2", "s2", "ext", 3, 3
  )
}
