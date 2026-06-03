test_that("HSL conversion returns hexadecimal colours", {
  testthat::expect_match(fearbase:::hsl_to_rgb(0, 0, 0), "^#[0-9A-F]{6}$")
  testthat::expect_identical(fearbase:::hsl_to_rgb(0, 0, 0), "#000000")
  testthat::expect_identical(fearbase:::hsl_to_rgb(0, 0, 1), "#FFFFFF")
})

test_that("HSL conversion covers hue and lightness branches", {
  colours <- vapply(
    c(0, 60, 120, 180, 240, 300, 360),
    fearbase:::hsl_to_rgb,
    character(1),
    s = 1,
    l = 0.25
  )

  light_colours <- vapply(
    c(30, 150, 270),
    fearbase:::hsl_to_rgb,
    character(1),
    s = 0.75,
    l = 0.75
  )

  testthat::expect_true(all(grepl("^#[0-9A-F]{6}$", colours)))
  testthat::expect_true(all(grepl("^#[0-9A-F]{6}$", light_colours)))
  testthat::expect_gt(length(unique(colours)), 3L)
})

test_that("palette generation uses expected branch lengths", {
  testthat::expect_length(fearbase:::generate_palette(1), 1L)
  testthat::expect_length(fearbase:::generate_palette(2), 2L)
  testthat::expect_length(fearbase:::generate_palette(3), 3L)
  testthat::expect_length(fearbase:::generate_palette(6), 6L)

  testthat::expect_true(
    all(grepl("^#[0-9A-F]{6}$", fearbase:::generate_palette(6)))
  )
})

test_that("package load hook applies ggplot defaults", {
  testthat::expect_no_error(fearbase:::.onLoad("", "fearbase"))
})
