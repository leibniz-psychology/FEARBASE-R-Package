test_that("sample size graph works", {
  sampleSizeByStudy() |> testthat::expect_s3_class("ggplot")
})
