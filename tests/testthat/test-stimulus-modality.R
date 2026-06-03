test_that("stimulus pie chart works", {
  stimulus_modality(fixture_metadata(), type = "us_type") |>
    testthat::expect_s3_class("ggplot")
})
test_that("stimulus pie chart works", {
  stimulus_modality(fixture_metadata(), type = "cs_type") |>
    testthat::expect_s3_class("ggplot")
})

test_that("stimulus modality supports participant totals and missing labels", {
  md <- fixture_metadata()
  md$us_type[1] <- ""

  graph <- stimulus_modality(md, type = "us_type", level = "n_subjects")

  testthat::expect_s3_class(graph, "ggplot")
  testthat::expect_true("not reported" %in% graph$data$modality)
})

test_that("stimulus modality validates selectors and numeric subject counts", {
  bad_subjects <- fixture_metadata()
  bad_subjects$n_subjects[1] <- "unknown"
  zero_subjects <- fixture_metadata()
  zero_subjects$n_subjects <- 0

  testthat::expect_error(
    stimulus_modality(fixture_metadata(), type = "context"),
    "`type` must be one of"
  )
  testthat::expect_error(
    stimulus_modality(fixture_metadata(), level = "conditions"),
    "`level` must be one of"
  )
  testthat::expect_error(stimulus_modality(bad_subjects), "numeric")
  testthat::expect_error(
    stimulus_modality(zero_subjects, level = "n_subjects"),
    "positive observation"
  )
})

test_that("stimulus modality resolves caller-side metadata", {
  metadata <- fixture_metadata()

  graph <- stimulus_modality()

  testthat::expect_s3_class(graph, "ggplot")
})
