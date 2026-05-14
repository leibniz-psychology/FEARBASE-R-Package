test_that("age histogram works", {
  age() |> testthat::expect_s3_class("ggplot")
})
test_that("age ridgeplot works", {
  age(type = "r") |> testthat::expect_s3_class("ggplot")
})
