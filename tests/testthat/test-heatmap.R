test_that("phases heatmap works", {
  phases_heatmap(fixture_long_data(), fixture_codebook()) |>
    testthat::expect_s3_class("ggplot")
})
test_that("measures heatmap works", {
  measures_heatmap(
    fixture_long_data(),
    fixture_metadata(),
    fixture_codebook()
  ) |>
    testthat::expect_s3_class("ggplot")
})

test_that("heatmap expands the left plot margin for long x labels", {
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

test_that("phase heatmap supports explicit exclusions and validates them", {
  phases_heatmap(
    fixture_long_data(),
    fixture_codebook(),
    exclude = "ext"
  ) |>
    testthat::expect_s3_class("ggplot")

  testthat::expect_equal(
    fearbase:::.validate_phase_exclude("none", c("hab", "acq")),
    character(0)
  )
  testthat::expect_equal(
    fearbase:::.validate_phase_exclude(c("hab", "hab"), c("hab", "acq")),
    "hab"
  )
  testthat::expect_error(
    fearbase:::.validate_phase_exclude(character(0), c("hab")),
    "non-empty character"
  )
  testthat::expect_error(
    fearbase:::.validate_phase_exclude(c("none", "hab"), c("hab")),
    "may contain \"none\" only"
  )
  testthat::expect_error(
    fearbase:::.validate_phase_exclude("missing", c("hab")),
    "Unknown value"
  )
})

test_that("co-occurrence data validates input shape and count type", {
  summary_data <- data.frame(
    category = c("a", "b", "a"),
    group = c("g1", "g1", "g2"),
    n = c(1, 2, 3)
  )

  co_occurrence <- fearbase:::.get_co_occurrence_data(
    summary_data,
    "category",
    "group",
    "n"
  )

  testthat::expect_named(co_occurrence, c("category", "category2", "value"))
  testthat::expect_error(
    fearbase:::.get_co_occurrence_data(data.frame(), "x", "y", "n"),
    "Missing required column"
  )
  testthat::expect_error(
    fearbase:::.get_co_occurrence_data(
      data.frame(x = "a", y = "b", n = "1"),
      "x",
      "y",
      "n"
    ),
    "numeric column"
  )
  testthat::expect_error(
    fearbase:::.get_co_occurrence_data(
      data.frame(x = character(), y = character(), n = numeric()),
      "x",
      "y",
      "n"
    ),
    "at least one row"
  )
})

test_that("co-occurrence heatmap validates values and handles all NA data", {
  all_na_data <- data.frame(
    x = c("a", "b"),
    y = c("a", "b"),
    value = c(NA_real_, NA_real_)
  )

  plot <- suppressWarnings(
    plot_co_occurrence_heatmap(all_na_data, "x", "y", "value")
  )

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_error(
    plot_co_occurrence_heatmap(
      data.frame(x = "a", y = "b", value = "1"),
      "x",
      "y",
      "value"
    ),
    "numeric column"
  )
  testthat::expect_error(
    plot_co_occurrence_heatmap(all_na_data, "x", "y", "value", diag_na = NA),
    "`diag_na` must be `TRUE` or `FALSE`"
  )
})

test_that("horizontal bar helper validates and supports fill groups", {
  bar_data <- data.frame(
    category = c("a", "b"),
    n = c(1, 2),
    group = c("g1", "g2")
  )

  filled_plot <- plot_horizontal_bar(bar_data, "category", "n", "group")

  testthat::expect_s3_class(filled_plot, "ggplot")
  testthat::expect_error(
    plot_horizontal_bar(data.frame(category = "a", n = "1"), "category", "n"),
    "numeric column"
  )
  testthat::expect_error(
    plot_horizontal_bar(
      data.frame(category = character(), n = numeric()),
      "category",
      "n"
    ),
    "at least one row"
  )
  testthat::expect_error(
    plot_horizontal_bar(bar_data, "category", "n", "missing"),
    "Missing required column"
  )
})

test_that("measure and phase heatmaps report empty mapped plotting data", {
  only_demographics <- subset(
    fixture_long_data(),
    measure %in% c("age", "sex")
  )
  no_plotted_phases <- subset(fixture_long_data(), phase == "hab")

  testthat::expect_error(
    measures_heatmap(only_demographics, fixture_metadata(), fixture_codebook()),
    "No mapped measure values"
  )
  testthat::expect_error(
    phases_heatmap(
      no_plotted_phases,
      fixture_codebook(),
      exclude = "hab"
    ),
    "No mapped phase values"
  )
})
