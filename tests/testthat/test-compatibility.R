test_that("deprecated camelCase wrappers delegate to snake_case implementations", {
  expect_deprecated_value <- function(expr) {
    value <- NULL
    testthat::expect_warning(
      value <- eval.parent(substitute(expr)),
      "deprecated"
    )
    value
  }

  md <- fixture_metadata()
  dl <- fixture_long_data()
  cb <- fixture_codebook()

  expect_deprecated_value(ageDescriptives(dl)) |>
    testthat::expect_s3_class("tbl_df")

  expect_deprecated_value(allStudies(md)) |>
    testthat::expect_type("character")

  lookup_env <- new.env(parent = emptyenv())
  lookup_env$payload <- data.frame(x = 1:3)
  expect_deprecated_value(checkData("payload", envir = lookup_env)) |>
    testthat::expect_s3_class("data.frame")

  csv_file <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(x = 1:2), csv_file, row.names = FALSE)
  expect_deprecated_value(createCsv(csv_file)) |>
    testthat::expect_s3_class("data.frame")

  json_payload <- data.frame(x = 1:2)
  expect_deprecated_value(jsonSummary("json_payload")) |>
    testthat::expect_type("character")

  expect_deprecated_value(dataCollectionYear(md)) |>
    testthat::expect_s3_class("ggplot")

  expect_deprecated_value(measuresHeatmap(dl, md, cb)) |>
    testthat::expect_s3_class("ggplot")

  expect_deprecated_value(peakDetectionWindows(md)) |>
    testthat::expect_s3_class("ggplot")

  expect_deprecated_value(phasesHeatmap(dl, cb)) |>
    testthat::expect_s3_class("ggplot")

  expect_deprecated_value(reinforcementRates(md)) |>
    testthat::expect_s3_class("ggplot")

  expect_deprecated_value(sampleSizeByStudy(dl)) |>
    testthat::expect_s3_class("ggplot")

  expect_deprecated_value(stimModality(md)) |>
    testthat::expect_s3_class("ggplot")

  diagnostic_plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 1),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()
  expect_deprecated_value(traceRemovedRows(diagnostic_plot)) |>
    testthat::expect_s3_class("tbl_df")

  expect_deprecated_value(trialsPhaseParticipant(dl, cb = cb)) |>
    testthat::expect_s3_class("ggplot")

  mapping_error <- NULL
  mapping_result <- NULL
  testthat::expect_warning(
    mapping_result <- tryCatch(
      updateMapping(assign_global = FALSE),
      error = function(error) {
        mapping_error <<- error
        NULL
      }
    ),
    "deprecated"
  )
  if (is.null(mapping_error)) {
    testthat::expect_s3_class(mapping_result, "data.frame")
  } else {
    testthat::expect_match(
      conditionMessage(mapping_error),
      "No internal mapping object"
    )
  }
})
