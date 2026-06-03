test_that("sample size graph works", {
  sample_size_by_study(fixture_long_data()) |>
    testthat::expect_s3_class("ggplot")
})

test_that("sample size graph supports condition grouping", {
  graph <- sample_size_by_study(
    fixture_long_data(),
    grouping_variable = "condition_id"
  )

  testthat::expect_s3_class(graph, "ggplot")
  testthat::expect_equal(graph$labels$x, "Condition ID")
})

test_that("sample size descriptives summarize participant counts", {
  summary <- fearbase:::sample_size_descriptives(fixture_long_data())

  testthat::expect_s3_class(summary, "data.frame")
  testthat::expect_equal(summary$n, 2)
})

test_that("sample size helpers validate grouping and empty counts", {
  missing_participant <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1"
  )
  missing_values <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1",
    participant_id = NA_character_
  )

  testthat::expect_error(
    sample_size_by_study(fixture_long_data(), grouping_variable = "paper_id"),
    "`grouping_variable` must be one of"
  )
  testthat::expect_error(
    sample_size_by_study(missing_participant),
    "Missing required column"
  )
  testthat::expect_error(
    sample_size_by_study(missing_values),
    "non-missing participant"
  )
})
