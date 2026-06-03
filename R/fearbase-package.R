#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

# Import only the external functions used unqualified in package code. Keeping
# imports explicit reduces namespace-clash risk while preserving the package's
# established tidyverse-oriented implementation style.
#' @importFrom dplyr across all_of any_of anti_join arrange bind_cols bind_rows
#' @importFrom dplyr case_when coalesce count desc distinct filter group_by
#' @importFrom dplyr if_else left_join
#' @importFrom dplyr mutate n n_distinct pull relocate rename select starts_with
#' @importFrom dplyr summarise
#' @importFrom ggplot2 aes coord_flip coord_polar element_blank element_rect
#' @importFrom ggplot2 element_text facet_grid geom_bar geom_col geom_hline
#' @importFrom ggplot2 geom_label geom_segment geom_text geom_tile ggplot
#' @importFrom ggplot2 ggplot_build guide_legend guide_none guides labs margin
#' @importFrom ggplot2 position_stack expansion rel
#' @importFrom ggplot2 scale_alpha scale_colour_manual scale_fill_discrete
#' @importFrom ggplot2 scale_fill_manual scale_x_continuous scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous scale_y_discrete set_theme theme
#' @importFrom ggplot2 theme_classic theme_void update_geom_defaults
#' @importFrom ggplot2 update_theme
#' @importFrom ggplot2 vars
#' @importFrom patchwork plot_layout
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom jsonlite toJSON
#' @importFrom scales extended_breaks pal_grey
#' @importFrom utils packageName read.csv
NULL
