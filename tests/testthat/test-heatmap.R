test_that("phases heatmap works", {
  phasesHeatmap() |> testthat::expect_s3_class("ggplot")
})
test_that("measures heatmap works", {
  measuresHeatmap() |> testthat::expect_s3_class("ggplot")
})
