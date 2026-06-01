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
