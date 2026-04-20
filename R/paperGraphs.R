Group1 <- function(folder = "paper/", m = 3) {
  sample <- sampleSizeByStudy() +
    geom_text(aes(label = n), hjust = -.3) +
    labs(title = "Sample Size")
  sex <- sex() +
    labs(title = "Sex Distribution")
  year <- dataCollectionYear() +
    # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Publication Year")
  p4 <- age(type = "r") +
    labs(title = "Age Distribution") +
    scale_x_continuous(breaks = seq(15, 60, 5))
  p5 <- age(type = "h") +
    theme(
      legend.position = "inside",
      legend.position.inside = c(.9, .5),
      legend.text = element_text(size = 4 * m)
    ) +
    guides(fill = guide_legend(ncol = 2, reverse = TRUE)) +
    labs(title = "Age Distribution") +
    scale_x_continuous(breaks = seq(15, 60, 5))

  update_geom_defaults("label", list(size = 4.5 * m / .pt))
  update_geom_defaults("text", list(size = 4.5 * m / .pt))

  plt <- ((year + sample + sex) +
    plot_layout(widths = c(2, 2, 1))) /
    (p4 + p5) +
    plot_annotation(tag_levels = 'A') &
    theme(text = element_text(size = 6 * m))

  ggsave(
    file.path(folder, "group1.png"),
    plot = plt,
    width = 6,
    height = 4.5,
    units = "in",
    scale = m,
    dpi = 96 * m
  )
}

Group2 <- function(folder = "paper/", m = 3) {
  p1 <- reinforcementRates() +
    # theme(text = element_text(size = 8 * m)) +
    coord_cartesian(ylim = c(0, 16)) +
    geom_text(aes(label = value), vjust = -.5) +
    labs(title = "Reinforcement Rate")
  p2 <- peakDetectionWindows() +
    labs(title = "SCR Response Quantification Approach") +
    theme(legend.position = "top") +
    coord_flip(ylim = c(-3, 8)) +
    scale_y_continuous(breaks = seq(-2, 8, 2))
  p3 <- instructions() +
    labs(title = "Contingency Instruction") +
    theme(plot.title = element_text(hjust = -1.35)) +
    labs(x = NULL)
  p4 <- stimModality(metadata, "cs_type", "n_studies") +
    labs(title = "CS Modality") +
    guides(fill = guide_legend(position = "top", title = ""))
  p5 <- stimModality(metadata, "us_type", "n_studies") +
    labs(title = "US Modality") +
    guides(fill = guide_legend(position = "top", title = ""))

  update_geom_defaults("label", list(size = 5 * m / .pt))
  update_geom_defaults("text", list(size = 5 * m / .pt))

  plt <- (free(p2) | p1 + theme(plot.tag.position = c(.05, .99))) /
    (p3 | ((p4 / p5) & theme(plot.tag.position = c(-.92, 1)))) +
    plot_annotation(tag_levels = 'A') &
    theme(text = element_text(size = 6 * m))

  ggsave(
    file.path(folder, "group2.png"),
    plot = plt,
    width = 6,
    height = 4.5,
    units = "in",
    scale = m,
    dpi = 96 * m
  )
}

Group3 <- function(folder = "paper/", m = 2) {
  p12 <- prepMeasuresHeatmap()
  p34 <- prepPhasesHeatmap()

  update_geom_defaults("label", list(size = 6 * m / .pt))

  p1 <- p12[[1]] +
    labs(title = "Combinations of Measures") +
    theme(text = element_text(size = 6 * m))
  p2 <- p12[[2]] +
    labs(title = "Number of\nParticipants")

  p3 <- p34[[1]] +
    labs(title = "Combinations of Phases") +
    theme(text = element_text(size = 6 * m))
  p4 <- p34[[2]] +
    labs(title = "Number of\nParticipants")

  plt <- (p1 + p2 + plot_layout(widths = c(1, 0.6))) /
    (p3 + p4 + plot_layout(widths = c(1, 0.6))) +
    plot_annotation(tag_levels = list(c('A', '', 'B', ''))) &
    theme(
      text = element_text(size = 11 * m),
      legend.text = element_text(size = 6 * m)
    )

  ggsave(
    file.path(folder, "group3.png"),
    plot = plt,
    width = 6,
    height = 8,
    units = "in",
    scale = m,
    dpi = 96 * m
  )
}

TPP <- function(folder = "paper/", m = 2) {
  ggsave(
    filename = file.path(folder, "trialsPerPhase.png"),
    plot = trialsPhaseParticipant(y_axis = "s") +
      theme(
        text = element_text(size = 14 * m)
      ) +
      labs(title = "Phase Lengths"),
    width = 6,
    height = 7,
    scale = m,
    dpi = 96 * m
  )
}
