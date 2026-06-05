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

test_that("FEARBASE theme helpers return ggplot themes", {
  testthat::expect_s3_class(theme_fearbase(), "theme")
  testthat::expect_s3_class(theme_fearbase_dense(), "theme")
  testthat::expect_s3_class(theme_fearbase_heatmap(), "theme")
  testthat::expect_s3_class(theme_fearbase_void(), "theme")
})

test_that("FEARBASE themes use IBM Plex Sans by default", {
  testthat::expect_true(theme_fearbase()$text$family %in% c(
    "IBM Plex Sans",
    "sans"
  ))
  testthat::expect_identical(
    theme_fearbase_dense()$text$family,
    theme_fearbase(base_size = 10)$text$family
  )
  testthat::expect_identical(
    theme_fearbase_void()$text$family,
    theme_fearbase(base_size = 11)$text$family
  )
})

test_that("FEARBASE theme helpers validate base size", {
  testthat::expect_error(
    theme_fearbase(base_size = 0),
    "`base_size` must be a single positive numeric value."
  )
  testthat::expect_error(
    theme_fearbase_void(base_size = NA_real_),
    "`base_size` must be a single positive numeric value."
  )
})

test_that("FEARBASE theme helpers validate and respect base family", {
  testthat::expect_identical(theme_fearbase(base_family = "")$text$family, "")
  testthat::expect_error(
    theme_fearbase(base_family = NA_character_),
    "`base_family` must be a single non-missing character string."
  )
  testthat::expect_error(
    theme_fearbase(base_family = c("IBM Plex Sans", "Arial")),
    "`base_family` must be a single non-missing character string."
  )
})

test_that("geom text size helper scales from points to millimetres", {
  small_size <- fearbase:::.fearbase_geom_text_size(base_size = 10, scale = 0.8)
  large_size <- fearbase:::.fearbase_geom_text_size(base_size = 12, scale = 0.8)

  testthat::expect_gt(small_size, 0)
  testthat::expect_gt(large_size, small_size)
})
