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
  testthat::expect_true(any(is.na(graph$data$instruction_contingency)))
  testthat::expect_equal(
    graph$data$n[is.na(graph$data$instruction_contingency)],
    2
  )
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
