test_that("phase length graph works", {
  trialsPhaseParticipant(fixture_long_data(), cb = fixture_codebook()) |>
    testthat::expect_s3_class("ggplot")
  trialsPhaseParticipant(
    fixture_long_data(),
    y_axis = "s",
    cb = fixture_codebook()
  ) |>
    testthat::expect_s3_class("ggplot")
  trialsPhaseParticipant(
    fixture_long_data(),
    y_axis = "s",
    grouping_variable = "paper_study_id",
    cb = fixture_codebook()
  ) |>
    testthat::expect_s3_class("ggplot")
  trialsPhaseParticipant(
    fixture_long_data(),
    y_axis = "s",
    grouping_variable = "study_id",
    cb = fixture_codebook()
  ) |>
    testthat::expect_s3_class("ggplot")
})
test_that("phase length graph (N source: study design) works", {
  studyDesign(fixture_study_design(), fixture_codebook()) |>
    testthat::expect_s3_class("ggplot")
})
