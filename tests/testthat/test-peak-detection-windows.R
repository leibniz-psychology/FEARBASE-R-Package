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
