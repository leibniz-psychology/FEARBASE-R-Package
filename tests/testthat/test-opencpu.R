test_that("check_data retrieves visible objects with validation", {
  lookup_env <- new.env(parent = emptyenv())
  lookup_env$payload <- data.frame(x = 1:3)

  testthat::expect_identical(
    check_data("payload", envir = lookup_env),
    lookup_env$payload
  )
})

test_that("check_data validates lookup arguments", {
  testthat::expect_error(check_data(NA_character_), "non-missing")
  testthat::expect_error(check_data("x", envir = 1), "valid environment")
  testthat::expect_error(check_data("x", inherits = NA), "logical scalar")
  testthat::expect_error(check_data("x", warn_as_error = NA), "logical scalar")
})

test_that("check_data reports missing objects", {
  lookup_env <- new.env(parent = emptyenv())

  testthat::expect_error(
    check_data("payload", envir = lookup_env),
    "not found"
  )
})

test_that("check_data can search parent environments", {
  parent_env <- new.env(parent = emptyenv())
  child_env <- new.env(parent = parent_env)
  parent_env$payload <- data.frame(x = 1)

  testthat::expect_identical(
    check_data("payload", envir = child_env, inherits = TRUE),
    parent_env$payload
  )
})

test_that("check_data can escalate or allow retrieval warnings", {
  lookup_env <- new.env(parent = emptyenv())
  makeActiveBinding(
    "payload",
    function() {
      warning("careful")
      data.frame(x = 1)
    },
    lookup_env
  )

  testthat::expect_error(
    check_data("payload", envir = lookup_env),
    "A warning occurred"
  )
  testthat::expect_warning(
    allowed <- check_data(
      "payload",
      envir = lookup_env,
      warn_as_error = FALSE
    ),
    "careful"
  )
  testthat::expect_s3_class(allowed, "data.frame")
})

test_that("check_data rejects missing inherited objects by default", {
  parent_env <- new.env(parent = emptyenv())
  child_env <- new.env(parent = parent_env)
  parent_env$payload <- data.frame(x = 1)

  testthat::expect_error(
    check_data("payload", envir = child_env),
    "not found"
  )
})

test_that("create_csv reads uploaded CSV files", {
  csv_file <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(x = 1:2), csv_file, row.names = FALSE)
  on.exit(unlink(csv_file), add = TRUE)

  parsed <- create_csv(csv_file)

  testthat::expect_s3_class(parsed, "data.frame")
  testthat::expect_equal(parsed$x, 1:2)
})

test_that("json_summary returns JSON for named data", {
  lookup_env <- new.env(parent = emptyenv())
  lookup_env$payload <- data.frame(x = 1:3)

  json <- json_summary("payload", envir = lookup_env)

  testthat::expect_type(json, "character")
  testthat::expect_true(jsonlite::validate(json))
  testthat::expect_match(json, "\"x\"")
})
