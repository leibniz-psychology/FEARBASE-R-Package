test_that("collection year bar graph works", {
  dataCollectionYear() |> testthat::expect_s3_class("ggplot")
})
