test_that("codebook resolver prefers explicit codebooks", {
  explicit_codebook <- fixture_codebook()
  caller_env <- new.env(parent = emptyenv())
  caller_env$codebook <- data.frame(x = 1)

  resolved <- fearbase:::.resolve_codebook(
    explicit_codebook,
    caller_env = caller_env
  )

  testthat::expect_identical(resolved, explicit_codebook)
})

test_that("codebook resolver uses caller-side codebooks", {
  caller_env <- new.env(parent = emptyenv())
  caller_env$codebook <- fixture_codebook()

  resolved <- fearbase:::.resolve_codebook(caller_env = caller_env)

  testthat::expect_identical(resolved, caller_env$codebook)
})

test_that("codebook resolver rejects missing codebooks", {
  caller_env <- new.env(parent = emptyenv())

  testthat::expect_error(
    fearbase:::.resolve_codebook(caller_env = caller_env),
    "`cb` must be supplied"
  )
})

test_that("codebook label mapping validates and formats labels", {
  mapping <- fearbase:::.get_codebook_label_mapping(
    fixture_codebook(),
    attribute = "measure",
    value_col = "measure_short",
    label_col = "measure_long"
  )

  testthat::expect_named(mapping, c("measure_short", "measure_long"))
  testthat::expect_true("Skin Conductance Response" %in% mapping$measure_long)
})

test_that("codebook label mapping can keep original case", {
  mapping <- fearbase:::.get_codebook_label_mapping(
    fixture_codebook(),
    attribute = "measure",
    value_col = "measure_short",
    label_col = "measure_long",
    title_case = FALSE
  )

  testthat::expect_true("skin conductance response" %in% mapping$measure_long)
})

test_that("codebook label mapping rejects absent attributes", {
  testthat::expect_error(
    fearbase:::.get_codebook_label_mapping(
      fixture_codebook(),
      attribute = "missing",
      value_col = "value",
      label_col = "label"
    ),
    "No `missing` rows"
  )
})
