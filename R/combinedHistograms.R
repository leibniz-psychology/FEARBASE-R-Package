#' Measures heatmap
#'
#' @description
#' Builds a combined heatmap and bar plot of measures.
#'
#' @return A ggplot object (patchwork).
#' @export
measuresHeatmap <- function() {
  plots <- prepMeasuresHeatmap()
  .arrange_histogram_layout(plots[[1]], plots[[2]])
}
prepMeasuresHeatmap <- function(dat = data_measures) {
  # Heatmap
  hm <- .plot_co_occurrence_heatmap(
    dat$heatmap_data,
    "measure",
    "measure2",
    "value",
    diag_na = TRUE
  )

  # Bar plot
  bp <- .plot_horizontal_bar(
    dat$barplot_data,
    "measure",
    "used",
    fill_var = "type"
  )

  # Combine
  return(c(hm, bp))
}

#' Phases heatmap
#'
#' @description
#' Builds a combined heatmap and bar plot of phases.
#'
#' @return A ggplot object (patchwork).
#' @export
phasesHeatmap <- function() {
  plots <- prepPhasesHeatmap(data_phases)
  .arrange_histogram_layout(plots[[1]], plots[[2]])
}

prepPhasesHeatmap <- function(dat = data_phases) {
  # Heatmap
  hm <- .plot_co_occurrence_heatmap(
    dat$heatmap_data,
    "phase",
    "phase2",
    "value",
    diag_na = TRUE
  )

  # Bar plot
  bp <- .plot_horizontal_bar(dat$barplot_data, "phase", "used")

  # Combine
  return(c(hm, bp))
}

# --- Internal Helper Functions ---
.plot_co_occurrence_heatmap <- function(
  df,
  x_var,
  y_var,
  value_var,
  diag_na = FALSE
) {
  if (diag_na) {
    df <- df |>
      mutate(
        !!value_var := ifelse(
          .data[[x_var]] == .data[[y_var]],
          NA,
          .data[[value_var]]
        )
      )
  }

  df <- df |>
    mutate(
      fill_white = ifelse(.data[[value_var]] > 0, 0, 1),
      fill_gray = ifelse(is.na(.data[[value_var]]), 1, 0),
      text_white = factor(
        ifelse(
          .data[[value_var]] > max(.data[[value_var]], na.rm = TRUE) / 4,
          "white",
          "black"
        ),
        levels = c("white", "black")
      )
    )

  ggplot(
    df,
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      fill = .data[[value_var]]
    )
  ) +
    geom_tile() +
    scale_y_discrete(position = "right") +
    geom_text(
      aes(label = .data[[value_var]]),
      # size = rel(3),
      color = "white",
      fontface = 'bold'
      # fill = "white"
    ) +
    geom_tile(
      aes(alpha = fill_white),
      fill = "white"
    ) +
    geom_tile(
      aes(alpha = fill_gray),
      fill = "gray50"
    ) +
    scale_color_manual(values = c("white", "black"), guide = "none") +
    scale_alpha(guide = "none") +
    labs(fill = "Number of Participants") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.key.width = unit(2, "line"),
      legend.title = element_text(margin = margin(r = 20)),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank()
    )
}

.plot_horizontal_bar <- function(df, cat_var, count_var, fill_var = NULL) {
  p <- ggplot(
    df,
    aes(x = .data[[cat_var]], y = .data[[count_var]])
  )

  if (!is.null(fill_var)) {
    p <- p +
      aes(fill = .data[[fill_var]]) +
      geom_col() +
      scale_fill_discrete(palette = scales::pal_grey())
  } else {
    p <- p + geom_col(fill = "gray50")
  }

  p +
    geom_text(
      aes(label = .data[[count_var]]),
      hjust = -.1,
      color = "black",
    ) +
    coord_flip(ylim = c(0, max(df[[count_var]]) * 1.2)) +
    theme_void() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
}

.arrange_histogram_layout <- function(hm, bp) {
  hm + bp + plot_layout(widths = c(1, 0.5))
}
