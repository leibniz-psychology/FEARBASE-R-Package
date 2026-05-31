test_that("stimulus pie chart works", {
  stimModality(fixture_metadata(), type = "us_type") |>
    testthat::expect_s3_class("ggplot")
})
test_that("stimulus pie chart works", {
  stimModality(fixture_metadata(), type = "cs_type") |>
    testthat::expect_s3_class("ggplot")
})
