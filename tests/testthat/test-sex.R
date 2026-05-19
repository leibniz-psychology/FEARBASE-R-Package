test_that("sex pie chart works", {
  sex() |> testthat::expect_s3_class("ggplot")
})
