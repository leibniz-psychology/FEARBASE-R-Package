test_that("long-data resolver requires explicit or caller-side data", {
  testthat::expect_error(
    fearbase:::.resolve_long_data(NULL),
    "`dl` must be supplied"
  )
})

test_that("caller-data resolver returns explicit and caller-side data", {
  explicit_data <- data.frame(x = 1)
  caller_env <- new.env(parent = emptyenv())
  caller_env$metadata <- data.frame(y = 2)

  testthat::expect_identical(
    fearbase:::.resolve_metadata(explicit_data, caller_env = caller_env),
    explicit_data
  )
  testthat::expect_identical(
    fearbase:::.resolve_metadata(caller_env = caller_env),
    caller_env$metadata
  )
})

test_that("caller-data resolver rejects non-data-frame caller objects", {
  caller_env <- new.env(parent = emptyenv())
  caller_env$data_long <- 1

  testthat::expect_error(
    fearbase:::.resolve_long_data(caller_env = caller_env),
    "must be a data frame"
  )
})

test_that("numeric coercion rejects malformed non-missing values", {
  testthat::expect_equal(
    fearbase:::.coerce_numeric_strict(c("1", "2", NA), "`x`"),
    c(1, 2, NA)
  )

  testthat::expect_error(
    fearbase:::.coerce_numeric_strict(c("1", "bad"), "`x`"),
    "numeric or coercible"
  )
})

test_that("validation helpers accept valid inputs invisibly", {
  testthat::expect_invisible(fearbase:::.validate_data_frame(data.frame(), "x"))
  testthat::expect_invisible(
    fearbase:::.validate_required_columns(data.frame(x = 1), "x", "data")
  )
  testthat::expect_invisible(
    fearbase:::.validate_single_column_name("x", "column")
  )
  testthat::expect_invisible(
    fearbase:::.validate_logical_scalar(TRUE, "flag")
  )
  testthat::expect_equal(
    fearbase:::.validate_choice("a", "choice", c("a", "b")),
    "a"
  )
})

test_that("validation helpers reject invalid inputs", {
  testthat::expect_error(
    fearbase:::.validate_data_frame(1, "x"),
    "must be a data frame"
  )
  testthat::expect_error(
    fearbase:::.validate_required_columns(data.frame(x = 1), "y", "data"),
    "Missing required column"
  )
  testthat::expect_error(
    fearbase:::.validate_single_column_name("", "column"),
    "single non-empty"
  )
  testthat::expect_error(
    fearbase:::.validate_logical_scalar(NA, "flag"),
    "`TRUE` or `FALSE`"
  )
  testthat::expect_error(
    fearbase:::.validate_choice("c", "choice", c("a", "b")),
    "must be one of"
  )
})

test_that("count-axis titles are centralized", {
  testthat::expect_equal(
    fearbase:::.count_axis_title("study_id"),
    "Number of Studies"
  )
  testthat::expect_equal(
    fearbase:::.count_axis_title("condition_id"),
    "Number of Conditions"
  )
  testthat::expect_equal(
    fearbase:::.count_axis_title("paper_study_id"),
    "Paper Study Id"
  )
})

test_that("expanded count limit handles finite and invalid inputs", {
  testthat::expect_equal(fearbase:::.expanded_count_limit(c(2, 4)), 4.8)
  testthat::expect_equal(
    suppressWarnings(fearbase:::.expanded_count_limit(numeric(0))),
    1
  )
  testthat::expect_equal(fearbase:::.expanded_count_limit(c(0, 0)), 1)
})

test_that("trace_removed_rows validates inputs", {
  plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  testthat::expect_error(
    trace_removed_rows(data.frame(x = 1)),
    "`plot` must be a ggplot object"
  )
  testthat::expect_error(
    trace_removed_rows(plot, data = 1),
    "`data` must be a data frame"
  )
  testthat::expect_error(
    trace_removed_rows(plot, layer = 0),
    "positive layer index"
  )
  testthat::expect_error(
    trace_removed_rows(plot, layer = 2),
    "larger than the number of plot layers"
  )
  testthat::expect_error(
    trace_removed_rows(plot, row_id_col = "x"),
    "already exists"
  )
})

test_that("trace_removed_rows reports removed source rows", {
  diagnostic_data <- data.frame(x = c(1, 10), y = c(1, 1))
  diagnostic_plot <- ggplot2::ggplot(
    diagnostic_data,
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    ggplot2::xlim(0, 5)

  removed_rows <- trace_removed_rows(diagnostic_plot)

  testthat::expect_equal(removed_rows$x, 10)
  testthat::expect_true(removed_rows$.removed)
  testthat::expect_match(removed_rows$.reason, "missing_x|outside_x_range")
})

test_that("trace_removed_rows rejects non 1:1 layer mappings", {
  histogram_data <- data.frame(x = c(1, 1, 2, 2, 3))
  histogram_plot <- ggplot2::ggplot(
    histogram_data,
    ggplot2::aes(x)
  ) +
    ggplot2::geom_histogram(binwidth = 1)

  testthat::expect_error(
    trace_removed_rows(histogram_plot),
    "does not map 1:1"
  )
})
