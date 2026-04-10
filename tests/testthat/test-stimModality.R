test_that("stimulus pie chart works", {
  stimModality(type = "us_type") |> testthat::expect_s3_class("ggplot")
})
test_that("stimulus pie chart works", {
  stimModality(type = "cs_type") |> testthat::expect_s3_class("ggplot")
})
