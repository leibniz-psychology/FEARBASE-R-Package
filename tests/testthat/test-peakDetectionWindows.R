test_that("peak detection windows graph works", {
  peakDetectionWindows(fixture_metadata()) |> testthat::expect_s3_class("ggplot")
})
