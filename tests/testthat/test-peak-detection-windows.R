test_that("peak detection windows graph works", {
  peak_detection_windows(fixture_metadata()) |> testthat::expect_s3_class("ggplot")
})
