test_that("peak detection windows graph works", {
  peak_detection_windows(fixture_metadata()) |>
    testthat::expect_s3_class("ggplot")
})

test_that("peak detection windows validate grouping and SCR columns", {
  testthat::expect_error(
    peak_detection_windows(1),
    "`md` must be a data frame"
  )
  testthat::expect_error(
    peak_detection_windows(
      fixture_metadata(),
      grouping_variable = NA_character_
    ),
    "single non-missing character"
  )
  testthat::expect_error(
    peak_detection_windows(fixture_metadata(), grouping_variable = "paper_id"),
    "`grouping_variable` must be one of"
  )
  testthat::expect_error(
    peak_detection_windows(data.frame(condition_id = "c1", study_id = "s1")),
    "complete supported set of SCR window"
  )
})

test_that("peak detection windows support legacy SCR columns", {
  md <- fixture_metadata()
  names(md) <- sub("^physio_", "", names(md))

  graph <- peak_detection_windows(md)

  testthat::expect_s3_class(graph, "ggplot")
})

test_that("peak detection windows reject empty numeric SCR intervals", {
  md <- fixture_metadata()
  md$physio_scr_baseline_window_start <- NA_real_
  md$physio_scr_peak_detection_window_min <- NA_real_

  testthat::expect_error(
    peak_detection_windows(md),
    "No complete SCR peak detection window rows"
  )
})

test_that("interactive peak detection windows group scoring definitions", {
  graph <- peak_detection_windows_interactive(fixture_metadata())

  testthat::expect_s3_class(graph, "girafe")

  testthat::expect_true(
    all(vapply(
      c("c1", "c2"),
      grepl,
      logical(1),
      graph[["x"]][["html"]],
      fixed = TRUE
    ))
  )

  testthat::expect_true(
    all(vapply(
      c("baseline_correction", "trough-to-peak"),
      grepl,
      logical(1),
      graph[["x"]][["html"]],
      fixed = TRUE
    ))
  )
})

test_that("interactive peak detection windows validate scalar inputs", {
  testthat::expect_error(
    peak_detection_windows_interactive(1),
    "`md` must be a data frame"
  )
  testthat::expect_error(
    peak_detection_windows_interactive(
      fixture_metadata(),
      grouping_variable = NA_character_
    ),
    "single non-missing character"
  )
  testthat::expect_error(
    peak_detection_windows_interactive(fixture_metadata(), save_html_widget = NA),
    "single non-missing logical"
  )
})

test_that("interactive peak detection windows support legacy SCR columns", {
  md <- fixture_metadata()
  names(md) <- sub("^physio_", "", names(md))

  graph <- peak_detection_windows_interactive(md)

  testthat::expect_s3_class(graph, "girafe")
})

test_that("interactive peak detection windows can save an HTML widget", {
  temporary_working_directory <- tempfile("peak-detection-widget-")

  dir.create(temporary_working_directory)
  on.exit(unlink(temporary_working_directory, recursive = TRUE), add = TRUE)

  withr::local_dir(temporary_working_directory)

  graph <- peak_detection_windows_interactive(
    fixture_metadata(),
    save_html_widget = TRUE
  )

  testthat::expect_s3_class(graph, "girafe")
  testthat::expect_true(file.exists("peak_detection_windows_interactive.html"))
  testthat::expect_true(dir.exists("peak_detection_windows_interactive_files"))
})
