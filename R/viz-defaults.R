fearbase_palette <- c(
  "#ffece2",
  "#ff8800",
  "#0032A0",
  "#0a1120"
)

fearbase_palette_v2 <- c(
  hsl_to_rgb(215.44, 1, .8),
  hsl_to_rgb(215.44, 1, .5745), #secondary
  hsl_to_rgb(221.25, 1, .3137), #primary
  hsl_to_rgb(221.25, 1, .2), #primary 800
  hsl_to_rgb(221.25, 1, .05)
)

generate_palette <- function(n_colors) {
  p <- fearbase_palette_v2
  if (n_colors == 1) {
    p[3]
  } else if (n_colors == 2) {
    p[c(2, 3)]
  } else if (n_colors == 3) {
    p[c(2, 3, 4)]
  } else {
    grDevices::colorRampPalette(
      p,
      space = "Lab",
      interpolate = "spline"
    )(n_colors)
  }
}

.onLoad <- function(libname, pkgname) {
  ggplot2::set_theme(ggplot2::theme_classic())

  ggplot2::update_theme(
    palette.colour.discrete = generate_palette,
    palette.fill.discrete = generate_palette,
    palette.fill.continuous = generate_palette(3),
    palette.color.continuous = generate_palette(3)
  )

  ggplot2::update_geom_defaults("bar", list(fill = generate_palette(1)))
  ggplot2::update_geom_defaults("boxplot", list(fill = generate_palette(1)))
}
