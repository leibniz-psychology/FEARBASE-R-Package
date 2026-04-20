#' Sex distribution
#'
#' @description
#' Generates a pie chart of the sex distribution.
#'
#' @param dl The data in long format.
#'
#' @return A ggplot object.
#' @export
sex <- function(dat = data_sex) {
  dat |>
    group_by(sex) |>
    summarise(n = n()) |>
    ggplot(aes(x = "", y = n, fill = sex)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    ggrepel::geom_label_repel(
      aes(label = paste0(sex, " (", n, ")"), group = sex),
      position = position_stack(vjust = 0.5),
      fill = "white"
    ) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}
