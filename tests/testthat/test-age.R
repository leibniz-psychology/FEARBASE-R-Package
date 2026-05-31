test_that("age histogram works", {
  age(fixture_long_data()) |> testthat::expect_s3_class("ggplot")
})
test_that("age ridgeplot works", {
  age(fixture_long_data(), type = "r") |> testthat::expect_s3_class("ggplot")
})
