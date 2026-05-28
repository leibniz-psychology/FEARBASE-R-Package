test_that("reinforcement rate graph works", {
  reinforcementRates() |> testthat::expect_s3_class("ggplot")
})

test_that("reinforcement rate graph counts unique studies by default", {
  md <- tibble::tibble(
    condition_id = c("c1", "c2", "c3", "c4"),
    study_id = c("s1", "s1", "s2", "s3"),
    reinf_a = c(50, 50, 75, NA),
    reinf_b = c(50, 60, 75, 80)
  )

  graph <- reinforcementRates(md)

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

  graph <- reinforcementRates(md, grouping_variable = "condition_id")

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
    reinforcementRates(md, grouping_variable = "participant_id"),
    "`grouping_variable` must be one of"
  )
})
