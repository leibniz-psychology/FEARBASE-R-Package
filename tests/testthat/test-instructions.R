test_that("instructions counts unique studies by default", {
  md <- data.frame(
    condition_id = c("c1", "c2", "c3", "c4"),
    study_id = c("s1", "s1", "s2", "s3"),
    instruction_contingency = c("aware", "aware", "aware", "masked")
  )

  graph <- instructions(md)

  testthat::expect_s3_class(graph, "ggplot")
  testthat::expect_identical(graph$labels$y, "Number of Studies")
  testthat::expect_equal(
    graph$data$n[graph$data$instruction_contingency == "aware"],
    2
  )
})

test_that("instructions counts unique conditions when requested", {
  md <- data.frame(
    condition_id = c("c1", "c2", "c3", "c3"),
    study_id = c("s1", "s1", "s2", "s2"),
    instruction_contingency = c("aware", "aware", "aware", "aware")
  )

  graph <- instructions(md, grouping_variable = "condition_id")

  testthat::expect_s3_class(graph, "ggplot")
  testthat::expect_identical(graph$labels$y, "Number of Conditions")
  testthat::expect_equal(graph$data$n, 3)
})

test_that("instructions keeps missing instruction values when requested", {
  md <- data.frame(
    condition_id = c("c1", "c2", "c3"),
    study_id = c("s1", "s2", "s3"),
    instruction_contingency = c("aware", NA, NA)
  )

  graph <- instructions(md, remove_na = FALSE)

  testthat::expect_s3_class(graph, "ggplot")
  testthat::expect_true("NA" %in% graph$data$instruction_contingency)
  testthat::expect_equal(
    graph$data$n[graph$data$instruction_contingency == "NA"],
    2
  )
})

test_that("instructions uses requested top-to-bottom display order", {
  requested_order <- c(
    "Fully instructed (whole exp)",
    "Partially instructed (whole exp)",
    "Different instructions in different conditions",
    "Different instructions in different phases",
    "Uninstructed (whole exp)",
    "NA"
  )

  md <- data.frame(
    condition_id = paste0("c", seq_along(requested_order)),
    study_id = paste0("s", seq_along(requested_order)),
    instruction_contingency = c(head(requested_order, -1), NA)
  )

  graph <- instructions(md, remove_na = FALSE)

  # coord_flip() renders factor levels from bottom to top, so the visual
  # top-to-bottom order is the reverse of the stored factor levels.
  testthat::expect_equal(
    rev(levels(graph$data$instruction_contingency)),
    requested_order
  )
})

test_that("instructions can sort categories by count", {
  md <- data.frame(
    condition_id = paste0("c", 1:6),
    study_id = paste0("s", 1:6),
    instruction_contingency = c(
      "Uninstructed (whole exp)",
      "Fully instructed (whole exp)",
      "Fully instructed (whole exp)",
      "Partially instructed (whole exp)",
      "Partially instructed (whole exp)",
      "Partially instructed (whole exp)"
    )
  )

  graph <- instructions(md, sort_by_count = TRUE)

  testthat::expect_equal(
    rev(levels(graph$data$instruction_contingency)),
    c(
      "Partially instructed (whole exp)",
      "Fully instructed (whole exp)",
      "Uninstructed (whole exp)"
    )
  )
  testthat::expect_equal(graph$data$n, c(1L, 2L, 3L))
})

test_that("instructions rejects unsupported grouping variables", {
  md <- data.frame(
    condition_id = "c1",
    study_id = "s1",
    instruction_contingency = "aware"
  )

  testthat::expect_error(
    instructions(md, grouping_variable = "paper_id"),
    "`grouping_variable` must be one of"
  )
})

test_that("instructions rejects invalid remove_na values", {
  md <- data.frame(
    condition_id = "c1",
    study_id = "s1",
    instruction_contingency = "aware"
  )

  testthat::expect_error(
    instructions(md, remove_na = NA),
    "`remove_na` must be a single non-missing logical value."
  )
})

test_that("instructions rejects invalid sort_by_count values", {
  md <- data.frame(
    condition_id = "c1",
    study_id = "s1",
    instruction_contingency = "Fully instructed (whole exp)"
  )

  testthat::expect_error(
    instructions(md, sort_by_count = NA),
    "`sort_by_count` must be a single non-missing logical value."
  )
})
