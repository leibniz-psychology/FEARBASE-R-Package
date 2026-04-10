test_that("phase length graph works", {
  trialsPhaseParticipant() |> testthat::expect_s3_class("ggplot")
  trialsPhaseParticipant(y_axis = "s") |> testthat::expect_s3_class("ggplot")
})
test_that("phase length graph (N source: study design) works", {
  studyDesign(study_design) |> testthat::expect_s3_class("ggplot")
})
