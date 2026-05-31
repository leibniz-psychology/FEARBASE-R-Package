test_that("sample size graph works", {
  sample_size_by_study(fixture_long_data()) |> testthat::expect_s3_class("ggplot")
})
