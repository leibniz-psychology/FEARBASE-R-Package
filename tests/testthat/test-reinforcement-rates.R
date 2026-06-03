test_that("reinforcement rate graph works", {
  reinforcement_rates(fixture_metadata()) |> testthat::expect_s3_class("ggplot")
})

test_that("reinforcement rate graph counts unique studies by default", {
  md <- tibble::tibble(
    condition_id = c("c1", "c2", "c3", "c4"),
    study_id = c("s1", "s1", "s2", "s3"),
    reinf_a = c(50, 50, 75, NA),
    reinf_b = c(50, 60, 75, 80)
  )

  graph <- reinforcement_rates(md)

  testthat::expect_equal(
    graph$data$n,
    c(1L, 1L, 1L, 1L)
  )
  testthat::expect_equal(graph$labels$y, "Number of Studies")
})

test_that("reinforcement rate graph counts unique conditions when requested", {
  md <- tibble::tibble(
    condition_id = c("c1", "c2", "c3", "c4"),
    study_id = c("s1", "s1", "s2", "s3"),
    reinf_a = c(50, 50, 75, NA),
    reinf_b = c(50, 60, 75, 80)
  )

  graph <- reinforcement_rates(md, grouping_variable = "condition_id")

  testthat::expect_equal(
    graph$data$n,
    c(2L, 1L, 1L, 1L)
  )
  testthat::expect_equal(graph$labels$y, "Number of Conditions")
})

test_that("reinforcement rate graph validates grouping variable", {
  md <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1",
    reinf_a = 50
  )

  testthat::expect_error(
    reinforcement_rates(md, grouping_variable = "participant_id"),
    "`grouping_variable` must be one of"
  )
})

test_that("reinforcement rate graph validates required values", {
  no_columns <- tibble::tibble(condition_id = "c1", study_id = "s1")
  all_missing <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1",
    reinf_a = NA_real_
  )

  testthat::expect_error(
    reinforcement_rates(no_columns),
    "at least one reinforcement-rate column"
  )
  testthat::expect_error(
    reinforcement_rates(all_missing),
    "at least one non-missing"
  )
})

test_that("reinforcement rate graph resolves caller-side metadata", {
  metadata <- fixture_metadata()

  graph <- reinforcement_rates()

  testthat::expect_s3_class(graph, "ggplot")
})

test_that("reinforcement rate graph rejects malformed values", {
  md <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1",
    reinf_a = "not numeric"
  )

  testthat::expect_error(
    reinforcement_rates(md),
    "numeric or coercible"
  )
})

test_that("reinforcement descriptives summarize non-missing values", {
  md <- tibble::tibble(
    condition_id = c("c1", "c2"),
    study_id = c("s1", "s2"),
    reinf_a = c(50, NA),
    reinf_b = c(75, 100)
  )

  summary <- fearbase:::reinforcement_rate_descriptives(md)

  testthat::expect_s3_class(summary, "data.frame")
  testthat::expect_equal(summary$n, 3)
})

test_that("reinforcement descriptives validate empty inputs", {
  no_columns <- tibble::tibble(condition_id = "c1", study_id = "s1")
  all_missing <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1",
    reinf_a = NA_real_
  )

  testthat::expect_error(
    fearbase:::reinforcement_rate_descriptives(no_columns),
    "at least one reinforcement-rate column"
  )
  testthat::expect_error(
    fearbase:::reinforcement_rate_descriptives(all_missing),
    "at least one non-missing"
  )
})
