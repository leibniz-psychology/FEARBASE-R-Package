test_that("phase length graph works", {
  ungrouped_graph <- trial_phase_counts(
    fixture_long_data(),
    cb = fixture_codebook()
  )

  ungrouped_graph |>
    testthat::expect_s3_class("ggplot")
  testthat::expect_false("fill" %in% names(ungrouped_graph$mapping))
  testthat::expect_false("group" %in% names(ungrouped_graph$mapping))

  grouped_participant_graph <- trial_phase_counts(
    fixture_long_data(),
    y_axis = "participants",
    grouping_variable = "condition_id",
    cb = fixture_codebook()
  )

  grouped_participant_graph |>
    testthat::expect_s3_class("ggplot")
  testthat::expect_true("fill" %in% names(grouped_participant_graph$mapping))
  testthat::expect_true("group" %in% names(grouped_participant_graph$mapping))

  testthat::expect_error(
    trial_phase_counts(
      fixture_long_data(),
      y_axis = "s",
      cb = fixture_codebook()
    ),
    "`grouping_variable` must be supplied"
  )

  trial_phase_counts(
    fixture_long_data(),
    y_axis = "s",
    grouping_variable = "paper_study_id",
    cb = fixture_codebook()
  ) |>
    testthat::expect_s3_class("ggplot")
  trial_phase_counts(
    fixture_long_data(),
    y_axis = "s",
    grouping_variable = "study_id",
    cb = fixture_codebook()
  ) |>
    testthat::expect_s3_class("ggplot")
})
test_that("phase length graph (N source: study design) works", {
  study_design_trial_counts(fixture_study_design(), fixture_codebook()) |>
    testthat::expect_s3_class("ggplot")
})

test_that("study-design trial counts validate empty plotted phases", {
  study_design <- data.frame(
    condition_id = "c1",
    study_id = "s1",
    name = "int",
    cspTrials = 1,
    csmTrials = 1
  )

  testthat::expect_error(
    study_design_trial_counts(study_design, fixture_codebook()),
    "No mapped study-design trial counts"
  )
})

test_that("trial-phase helpers validate unsupported selectors", {
  testthat::expect_error(
    trial_phase_counts(
      fixture_long_data(),
      y_axis = "records",
      cb = fixture_codebook()
    ),
    "`y_axis` must be one of"
  )
  testthat::expect_error(
    trial_phase_counts(
      fixture_long_data(),
      grouping_variable = "paper_id",
      cb = fixture_codebook()
    ),
    "`grouping_variable` must be one of"
  )
})

test_that("trial-phase descriptives reuse prepared trial counts", {
  result <- fearbase:::trial_phase_count_descriptives(
    fixture_long_data(),
    grouping_variable = "condition_id",
    cb = fixture_codebook()
  )

  testthat::expect_true(is.list(result))
  testthat::expect_true(length(result) > 0L)
})
