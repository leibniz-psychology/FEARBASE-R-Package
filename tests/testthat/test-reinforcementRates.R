test_that("reinforcement rate graph works", {
  reinforcementRates() |> testthat::expect_s3_class("ggplot")
})
