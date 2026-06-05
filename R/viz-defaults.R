fearbase_palette <- c(
  "#ffece2",
  "#ff8800",
  "#0032A0",
  "#0a1120"
)

fearbase_palette_v2 <- c(
  hsl_to_rgb(215.44, 1, 0.8),
  hsl_to_rgb(215.44, 1, 0.5745), #secondary
  hsl_to_rgb(221.25, 1, 0.3137), #primary
  hsl_to_rgb(221.25, 1, 0.2), #primary 800
  hsl_to_rgb(221.25, 1, 0.05)
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

.fearbase_default_base_family <- "IBM Plex Sans"

#' FEARBASE ggplot2 theme
#'
#' @description
#' Applies the standard FEARBASE visual theme for ordinary ggplot2 statistical
#' charts. The theme uses a classic panel structure, consistent relative text
#' sizing, compact margins, the IBM Plex Sans base font family, and the
#' package's preferred top legend placement.
#'
#' @param base_size A positive numeric scalar giving the base text size in
#'   points.
#' @param base_family A single character string naming the base font family.
#'   Defaults to `"IBM Plex Sans"`. When the optional `systemfonts` package is
#'   available with `sysfonts` and `showtext`, FEARBASE attempts to register the
#'   installed font before building the theme. If registration is not available,
#'   the default family falls back to `"sans"`.
#' @param legend_position A single character string or numeric vector accepted
#'   by \code{\link[ggplot2:theme]{ggplot2::theme()}} for
#'   `legend.position`.
#'
#' @return A \code{\link[ggplot2:theme]{ggplot2 theme}} object.
#'
#' @examples
#' \dontrun{
#' sample_size_by_study(data_long) + theme_fearbase(base_size = 12)
#' }
#'
#' @export
theme_fearbase <- function(
  base_size = 14,
  base_family = .fearbase_default_base_family,
  legend_position = "top"
) {
  .validate_fearbase_base_size(base_size)
  base_family <- .fearbase_import_font(base_family)

  # Build from theme_classic() to preserve the package's existing visual
  # contract while centralizing typography, legend, and margin choices.
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.title = element_text(size = rel(1)),
      axis.text = element_text(size = rel(0.85)),
      legend.position = legend_position,
      legend.title = element_text(size = rel(0.95)),
      legend.text = element_text(size = rel(0.85)),
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(size = rel(0.95), face = "bold"),
      plot.title = element_text(size = rel(1.2), face = "bold"),
      plot.subtitle = element_text(size = rel(1)),
      plot.caption = element_text(size = rel(0.8)),
      plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt")
    )
}

#' Dense FEARBASE ggplot2 theme
#'
#' @description
#' Applies a compact FEARBASE theme for plots with many categories, facets, or
#' long labels. It keeps the same visual structure as \code{theme_fearbase()}
#' while reducing text sizes and tightening legend spacing.
#'
#' @inheritParams theme_fearbase
#'
#' @return A \code{\link[ggplot2:theme]{ggplot2 theme}} object.
#'
#' @examples
#' \dontrun{
#' trial_phase_counts(data_long, cb = codebook) + theme_fearbase_dense()
#' }
#'
#' @export
theme_fearbase_dense <- function(
  base_size = 10,
  base_family = .fearbase_default_base_family,
  legend_position = "top"
) {
  theme_fearbase(
    base_size = base_size,
    base_family = base_family,
    legend_position = legend_position
  ) +
    theme(
      axis.text = element_text(size = rel(0.8)),
      legend.title = element_text(size = rel(0.9)),
      legend.text = element_text(size = rel(0.8)),
      strip.text = element_text(size = rel(0.9), face = "bold")
    )
}

#' FEARBASE heatmap theme
#'
#' @description
#' Applies FEARBASE styling for tile heatmaps. The theme keeps labels compact,
#' rotates x-axis text, places y-axis labels on the right when requested by the
#' scale, and removes axis titles because the tile coordinates already encode
#' the plotted categories.
#'
#' @inheritParams theme_fearbase
#'
#' @return A \code{\link[ggplot2:theme]{ggplot2 theme}} object.
#'
#' @examples
#' \dontrun{
#' plot_co_occurrence_heatmap(df, "measure_1", "measure_2", "n") +
#'   theme_fearbase_heatmap()
#' }
#'
#' @export
theme_fearbase_heatmap <- function(
  base_size = 10,
  base_family = .fearbase_default_base_family,
  legend_position = "top"
) {
  theme_fearbase_dense(
    base_size = base_size,
    base_family = base_family,
    legend_position = legend_position
  ) +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = rel(0.8)
      ),
      axis.text.y = element_text(size = rel(0.8)),
      panel.background = element_rect(fill = "white", color = NA),
      legend.key.width = grid::unit(2, "line"),
      legend.title = element_text(
        size = rel(0.9),
        margin = margin(r = 20)
      )
    )
}

#' Void FEARBASE ggplot2 theme
#'
#' @description
#' Applies a FEARBASE variant of \code{\link[ggplot2:theme_void]{theme_void()}}
#' for pies and layout component plots where axes and panels would add visual
#' clutter.
#'
#' @inheritParams theme_fearbase
#' @param background A single colour string used for the plot background.
#'
#' @return A \code{\link[ggplot2:theme]{ggplot2 theme}} object.
#'
#' @examples
#' \dontrun{
#' sex(data_long) + theme_fearbase_void(background = "transparent")
#' }
#'
#' @export
theme_fearbase_void <- function(
  base_size = 11,
  base_family = .fearbase_default_base_family,
  legend_position = "none",
  background = "white"
) {
  .validate_fearbase_base_size(base_size)
  base_family <- .fearbase_import_font(base_family)

  theme_void(base_size = base_size, base_family = base_family) +
    theme(
      legend.position = legend_position,
      legend.title = element_text(size = rel(0.95)),
      legend.text = element_text(size = rel(0.85)),
      plot.background = element_rect(fill = background, color = NA),
      plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt")
    )
}

.validate_fearbase_base_size <- function(base_size) {
  # Theme and geom sizing both depend on a single positive base size, so validate
  # that contract once and reuse it across the visualization helpers.
  if (
    !is.numeric(base_size) ||
      length(base_size) != 1L ||
      is.na(base_size) ||
      base_size <= 0
  ) {
    stop("`base_size` must be a single positive numeric value.", call. = FALSE)
  }

  invisible(base_size)
}

.validate_fearbase_base_family <- function(base_family) {
  # Keep the font contract strict because ggplot themes receive exactly one
  # family string. Empty strings remain valid so callers can explicitly request
  # the graphics-device default.
  if (
    !is.character(base_family) ||
      length(base_family) != 1L ||
      is.na(base_family)
  ) {
    stop("`base_family` must be a single non-missing character string.",
      call. = FALSE
    )
  }

  invisible(base_family)
}

.fearbase_import_font <- function(
  base_family = .fearbase_default_base_family
) {
  .validate_fearbase_base_family(base_family)

  # An empty family is the ggplot2/device default. Respect it and avoid font
  # lookup work when callers intentionally opt out of the FEARBASE font.
  if (identical(base_family, "")) {
    return(base_family)
  }

  # The default FEARBASE font needs showtext/sysfonts to render reliably on
  # PDF/PostScript devices. If callers request another family, pass it through
  # and let the active graphics device decide how to resolve it.
  if (!identical(base_family, .fearbase_default_base_family)) {
    return(base_family)
  }

  if (
    !requireNamespace("systemfonts", quietly = TRUE) ||
      !requireNamespace("sysfonts", quietly = TRUE) ||
      !requireNamespace("showtext", quietly = TRUE)
  ) {
    return("sans")
  }

  installed_fonts <- tryCatch(
    systemfonts::system_fonts(),
    error = function(error) {
      NULL
    }
  )

  if (is.null(installed_fonts) || nrow(installed_fonts) == 0L) {
    return("sans")
  }

  family_fonts <- installed_fonts[
    .fearbase_matches_font_family(installed_fonts$family, base_family),
  ]

  if (nrow(family_fonts) == 0L) {
    return("sans")
  }

  plain_path <- .fearbase_font_path(family_fonts, c("Regular", "Book", "Text"))
  if (is.na(plain_path)) {
    plain_path <- family_fonts$path[[1L]]
  }

  bold_path <- .fearbase_font_path(family_fonts, c("Bold", "SemiBold"))
  italic_path <- .fearbase_font_path(family_fonts, c("Italic", "Oblique"))
  bold_italic_path <- .fearbase_font_path(
    family_fonts,
    c("Bold Italic", "Bold Oblique", "SemiBold Italic", "SemiBold Oblique")
  )

  bold_path <- .fearbase_font_path_or_plain(bold_path, plain_path)
  italic_path <- .fearbase_font_path_or_plain(italic_path, plain_path)
  bold_italic_path <- .fearbase_font_path_or_plain(
    bold_italic_path,
    plain_path
  )

  # Register all faces under the same family twice: systemfonts supports modern
  # graphics devices, while showtext/sysfonts makes the same family usable on
  # PDF/PostScript devices used by examples, vignettes, and package checks.
  registration_succeeded <- tryCatch(
    {
      systemfonts::register_font(
        name = base_family,
        plain = plain_path,
        bold = bold_path,
        italic = italic_path,
        bolditalic = bold_italic_path
      )
      sysfonts::font_add(
        family = base_family,
        regular = plain_path,
        bold = bold_path,
        italic = italic_path,
        bolditalic = bold_italic_path
      )
      showtext::showtext_auto(enable = TRUE)
      TRUE
    },
    error = function(error) {
      FALSE
    }
  )

  if (registration_succeeded) {
    return(base_family)
  }

  "sans"
}

.fearbase_font_path <- function(fonts, preferred_styles) {
  style_match <- fonts$style %in% preferred_styles

  if (any(style_match)) {
    return(fonts$path[which(style_match)[[1L]]])
  }

  NA_character_
}

.fearbase_font_path_or_plain <- function(font_path, plain_path) {
  if (is.na(font_path)) {
    return(plain_path)
  }

  font_path
}

.fearbase_matches_font_family <- function(x, value) {
  !is.na(x) & x == value
}

.fearbase_geom_text_size <- function(base_size = 11, scale = 0.85) {
  .validate_fearbase_base_size(base_size)

  if (
    !is.numeric(scale) ||
      length(scale) != 1L ||
      is.na(scale) ||
      scale <= 0
  ) {
    stop("`scale` must be a single positive numeric value.", call. = FALSE)
  }

  # ggplot2 theme text uses points, whereas geom_text()/geom_label() use
  # millimetres. Converting here lets labels stay visually relative to the theme
  # base size without relying on ggplot2's internal `.pt` constant.
  base_size * scale / (72.27 / 25.4)
}

.onLoad <- function(libname, pkgname) {
  set_theme(theme_fearbase())

  update_theme(
    palette.colour.discrete = generate_palette,
    palette.fill.discrete = generate_palette,
    palette.fill.continuous = generate_palette(3),
    palette.color.continuous = generate_palette(3)
  )

  update_geom_defaults("bar", list(fill = generate_palette(1)))
  update_geom_defaults("boxplot", list(fill = generate_palette(1)))
}
