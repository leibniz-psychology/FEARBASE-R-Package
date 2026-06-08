test_that("age histogram works", {
  age(fixture_long_data()) |> testthat::expect_s3_class("ggplot")
  age(
    fixture_long_data(),
    grouping_variable = "study_id"
  ) |>
    testthat::expect_s3_class("ggplot")
})
test_that("age ridgeplot works", {
  age(fixture_long_data(), type = "r") |> testthat::expect_s3_class("ggplot")
  age(
    fixture_long_data(),
    type = "r",
    grouping_variable = "condition_id"
  ) |>
    testthat::expect_s3_class("ggplot")
})

test_that("age plot validates inputs and grouping choices", {
  testthat::expect_error(age(data.frame()), "Missing required column")
  testthat::expect_error(
    age(fixture_long_data(), type = "scatter"),
    "`type` must be one of"
  )
  testthat::expect_error(
    age(fixture_long_data(), grouping_variable = "participant_id"),
    "`grouping_variable` must be one of"
  )
})

test_that("age descriptives support global and grouped summaries", {
  overall <- age_descriptives(fixture_long_data())
  grouped <- age_descriptives(
    fixture_long_data(),
    grouping_variable = "study_id"
  )

  testthat::expect_equal(overall$n, 4L)
  testthat::expect_named(
    overall,
    c("mean_age", "sd_age", "min_age", "max_age", "n")
  )
  testthat::expect_true("study_id" %in% names(grouped))
  testthat::expect_equal(sum(grouped$n), 4L)
})

test_that("age descriptives validate grouping and age values", {
  no_age <- subset(fixture_long_data(), measure != "age")

  testthat::expect_error(
    age_descriptives(fixture_long_data(), grouping_variable = 1),
    "character vector"
  )
  testthat::expect_error(
    age_descriptives(fixture_long_data(), grouping_variable = "missing"),
    "not found"
  )
  testthat::expect_error(age_descriptives(no_age), "No valid age data")
})
