test_that("long-data resolver requires explicit or caller-side data", {
  testthat::expect_error(
    fearbase:::.resolve_long_data(NULL),
    "`dl` must be supplied"
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

test_that("count-axis titles are centralized", {
  testthat::expect_equal(
    fearbase:::.count_axis_title("study_id"),
    "Number of Studies"
  )
  testthat::expect_equal(
    fearbase:::.count_axis_title("condition_id"),
    "Number of Conditions"
  )
})
