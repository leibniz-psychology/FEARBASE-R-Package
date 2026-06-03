test_that("peak detection windows graph works", {
  peak_detection_windows(fixture_metadata()) |> testthat::expect_s3_class("ggplot")
})

test_that("dynamic peak detection windows group scoring definitions", {
  graph <- peak_detection_windows_dynamic(fixture_metadata())

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

test_that("dynamic peak detection windows can save an HTML widget", {
  original_working_directory <- getwd()
  temporary_working_directory <- tempfile("peak-detection-widget-")

  dir.create(temporary_working_directory)
  on.exit(setwd(original_working_directory), add = TRUE)
  on.exit(unlink(temporary_working_directory, recursive = TRUE), add = TRUE)

  setwd(temporary_working_directory)

  graph <- peak_detection_windows_dynamic(
    fixture_metadata(),
    save_html_widget = TRUE
  )

  testthat::expect_s3_class(graph, "girafe")
  testthat::expect_true(file.exists("peak_detection_windows_dynamic.html"))
  testthat::expect_true(dir.exists("peak_detection_windows_dynamic_files"))
})
