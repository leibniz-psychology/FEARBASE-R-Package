test_that("preferred exported function names use tidyverse-style snake_case", {
  namespace_path <- if (file.exists("NAMESPACE")) {
    "NAMESPACE"
  } else {
    system.file("NAMESPACE", package = "fearbase")
  }
  if (identical(namespace_path, "")) {
    namespace_path <- file.path("..", "..", "NAMESPACE")
  }
  namespace_lines <- readLines(namespace_path, warn = FALSE)
  exported_names <- sub(
    "^export\\(([^)]+)\\)$",
    "\\1",
    grep("^export\\(", namespace_lines, value = TRUE)
  )

  compatibility_exports <- c(
    "ageDescriptives",
    "allStudies",
    "checkData",
    "createCsv",
    "dataCollectionYear",
    "jsonSummary",
    "measuresHeatmap",
    "peakDetectionWindows",
    "phasesHeatmap",
    "reinforcementRates",
    "sampleSizeByStudy",
    "stimModality",
    "traceRemovedRows",
    "trialsPhaseParticipant",
    "updateMapping"
  )
  preferred_exports <- setdiff(exported_names, compatibility_exports)

  testthat::expect_true(
    all(grepl("^[a-z][a-z0-9_]*$", preferred_exports)),
    info = paste(
      "Non-snake_case preferred exports:",
      paste(
        preferred_exports[!grepl("^[a-z][a-z0-9_]*$", preferred_exports)],
        collapse = ", "
      )
    )
  )
})

test_that("primary implementation files do not define camelCase functions", {
  source_dir <- if (dir.exists("R")) {
    "R"
  } else {
    file.path("..", "..", "R")
  }
  if (!dir.exists(source_dir)) {
    testthat::skip("Package source files are not available in this test context.")
  }
  implementation_files <- setdiff(
    list.files(source_dir, pattern = "\\.R$", full.names = TRUE),
    file.path(source_dir, "compatibility.R")
  )
  definition_lines <- unlist(lapply(implementation_files, function(path) {
    lines <- readLines(path, warn = FALSE)
    matches <- grep(
      "^[A-Za-z_.][A-Za-z0-9_.]*\\s*<-\\s*function",
      lines,
      value = TRUE
    )

    paste(path, matches, sep = ":")
  }))
  definition_lines <- definition_lines[
    grepl("<-\\s*function", definition_lines)
  ]
  definition_names <- sub(
    "^([^:]+):([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-\\s*function.*$",
    "\\2",
    definition_lines
  )
  definition_names <- setdiff(definition_names, ".onLoad")
  camel_case_names <- definition_names[
    grepl("[a-z][A-Za-z0-9_.]*[A-Z]", definition_names)
  ]

  testthat::expect_equal(camel_case_names, character(0))
})

test_that("deprecated camelCase implementations live only in compatibility.R", {
  compatibility_names <- c(
    "ageDescriptives",
    "allStudies",
    "checkData",
    "createCsv",
    "dataCollectionYear",
    "jsonSummary",
    "measuresHeatmap",
    "peakDetectionWindows",
    "phasesHeatmap",
    "reinforcementRates",
    "sampleSizeByStudy",
    "stimModality",
    "traceRemovedRows",
    "trialsPhaseParticipant",
    "updateMapping"
  )

  source_dir <- if (dir.exists("R")) {
    "R"
  } else {
    file.path("..", "..", "R")
  }
  if (!dir.exists(source_dir)) {
    testthat::skip("Package source files are not available in this test context.")
  }
  implementation_files <- setdiff(
    list.files(source_dir, pattern = "\\.R$", full.names = TRUE),
    file.path(source_dir, "compatibility.R")
  )
  implementation_text <- paste(
    unlist(lapply(implementation_files, readLines, warn = FALSE)),
    collapse = "\n"
  )

  for (function_name in compatibility_names) {
    testthat::expect_false(
      grepl(
        paste0("\\b", function_name, "\\s*<-\\s*function"),
        implementation_text
      ),
      info = paste(function_name, "should only be defined in R/compatibility.R")
    )
  }
})
