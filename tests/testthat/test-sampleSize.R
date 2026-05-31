test_that("sample size graph works", {
  sampleSizeByStudy(fixture_long_data()) |> testthat::expect_s3_class("ggplot")
})
