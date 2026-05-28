test_that("collection year bar graph works", {
  dataCollectionYear() |> testthat::expect_s3_class("ggplot")
})

test_that("collection year graph counts unique studies by default", {
  md <- tibble::tibble(
    condition_id = c("c1", "c2", "c3", "c4", "c4"),
    study_id = c("s1", "s1", "s2", "s3", "s3"),
    year = c(2020, 2020, 2021, 2021, 2021)
  )

  graph <- dataCollectionYear(md)

  testthat::expect_equal(graph$data$year, c(2020, 2021))
  testthat::expect_equal(graph$data$n, c(1L, 2L))
  testthat::expect_equal(graph$labels$x, "Year of Publication")
  testthat::expect_equal(graph$labels$y, "Number of Studies")
})

test_that("collection year graph counts unique conditions when requested", {
  md <- tibble::tibble(
    condition_id = c("c1", "c2", "c3", "c4", "c4"),
    study_id = c("s1", "s1", "s2", "s3", "s3"),
    year = c(2020, 2020, 2021, 2021, 2021)
  )

  graph <- dataCollectionYear(md, grouping_variable = "condition_id")

  testthat::expect_equal(graph$data$year, c(2020, 2021))
  testthat::expect_equal(graph$data$n, c(2L, 2L))
  testthat::expect_equal(graph$labels$y, "Number of Conditions")
})

test_that("collection year graph uses data year when requested", {
  md <- tibble::tibble(
    condition_id = c("c1", "c2", "c3", "c4"),
    study_id = c("s1", "s1", "s2", "s3"),
    year = c(2020, 2020, 2021, 2022),
    year_data = c(2018, 2018, 2019, 2019)
  )

  graph <- dataCollectionYear(md, year_of = "data")

  testthat::expect_equal(graph$data$year, c(2018, 2019))
  testthat::expect_equal(graph$data$n, c(1L, 2L))
  testthat::expect_equal(graph$labels$x, "Year of Data Uplod")
  testthat::expect_equal(graph$labels$y, "Number of Studies")
})

test_that("collection year graph validates grouping variable", {
  md <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1",
    year = 2020
  )

  testthat::expect_error(
    dataCollectionYear(md, grouping_variable = "participant_id"),
    "`grouping_variable` must be one of"
  )
})

test_that("collection year graph validates year selector", {
  md <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1",
    year = 2020,
    year_data = 2019
  )

  testthat::expect_error(
    dataCollectionYear(md, year_of = "collection"),
    "`year_of` must be one of"
  )
})
