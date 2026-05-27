test_that("phases heatmap works", {
  phasesHeatmap() |> testthat::expect_s3_class("ggplot")
})
test_that("measures heatmap works", {
  measuresHeatmap() |> testthat::expect_s3_class("ggplot")
})

test_that("co-occurrence heatmap expands the left plot margin for long x labels", {
  heatmap_data <- data.frame(
    measure_x = c(
      "Short",
      "A very long outcome measure label that needs additional room"
    ),
    measure_y = c(
      "A very long outcome measure label that needs additional room",
      "Short"
    ),
    value = c(1, 2)
  )

  heatmap_plot <- plot_co_occurrence_heatmap(
    heatmap_data,
    "measure_x",
    "measure_y",
    "value"
  )

  left_margin_points <- as.numeric(heatmap_plot$theme$plot.margin[4])

  expect_gt(left_margin_points, 5.5)
})
