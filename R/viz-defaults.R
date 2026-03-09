fearbase_palette <- c(
  "#ffece2",
  "#ff8800",
  "#0032A0"
)

generate_palette <- function(n_colors) {
  if (n_colors == 1) {
    fearbase_palette[3]
  } else if (n_colors == 2) {
    fearbase_palette[c(3, 2)]
  } else if (n_colors == 3) {
    fearbase_palette[c(3, 2, 1)]
  } else {
    grDevices::colorRampPalette(
      fearbase_palette,
      space = "Lab",
      interpolate = "spline"
    )(n_colors)
  }
}

.onLoad <- function(libname, pkgname) {
  # These settings only apply when the package is loaded in a session
  set_theme(theme_classic())

update_theme(
    palette.colour.discrete = generate_palette,
    palette.fill.discrete = generate_palette,
    palette.fill.continuous = fearbase_palette,
    palette.color.continuous = fearbase_palette
  )
}
