test_that("peak detection windows graph works", {
  peakDetectionWindows() |> testthat::expect_s3_class("ggplot")
})
