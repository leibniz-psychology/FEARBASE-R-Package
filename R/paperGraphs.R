#' @import patchwork
Group1 <- function(folder = "paper/", m = 3) {
  p1 <- sampleSizeByStudy() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    geom_text(aes(label = n), vjust = -.5) +
    labs(title = "Sample Size by Study")
  p2 <- sex() +
    labs(title = "Sex Distribution")
  p3 <- dataCollectionYear() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Publication Year")
  p4 <- age("r") +
    labs(title = "Age Distribution by Study")
  p5 <- age("h") +
    theme(
      legend.position = "inside",
      legend.position.inside = c(.9, .5),
      legend.text = element_text(size = 4 * m)
    ) +
    labs(title = "Age Distribution")

  library(patchwork)
  update_geom_defaults("label", list(size = 5 * m / .pt))
  update_geom_defaults("text", list(size = 5 * m / .pt))

  plt <- ((p1 + p2 + p3) +
    plot_layout(widths = c(2, 1, 2))) /
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
    labs(title = "Reinforcement Rate Histogram")
  p2 <- peakDetectionWindows() +
    labs(title = "Scoring Methods") +
    theme(legend.position = "top")
  p3 <- instructions() +
    labs(title = "Contingency Instruction")
  p4 <- stimModality(metadata, "cs_type", "n_studies") +
    labs(title = "Conditioned Stimulus Modality")
  p5 <- stimModality(metadata, "us_type", "n_studies") +
    labs(title = "Unconditioned Stimulus Modality")

  update_geom_defaults("label", list(size = 5 * m / .pt))
  update_geom_defaults("text", list(size = 5 * m / .pt))

  plt <- (free(p2) | p1) /
    (p3 + (p4 / p5)) +
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

  plt <- (p1 + p2 + plot_layout(widths = c(1, 0.5))) /
    (p3 + p4 + plot_layout(widths = c(1, 0.5))) +
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

TPPalternatives <- function(folder = "paper/TPPalternatives/", m = 2) {
  ggsave(
    filename = file.path(folder, "1.png"),
    plot = trialsPhaseParticipant("s") +
      theme(
        text = element_text(size = 14 * m)
      ) +
      labs(title = "Phase Lengths 1"),
    width = 6,
    height = 7,
    scale = m,
    dpi = 96 * m
  )

  ggsave(
    filename = file.path(folder, "2.png"),
    plot = trialsPhaseParticipant("s", TRUE) +
      theme(
        text = element_text(size = 14 * m)
      ) +
      labs(title = "Phase Lengths 2"),
    width = 6,
    height = 7,
    scale = m,
    dpi = 96 * m
  )
  ggsave(
    filename = file.path(folder, "3.png"),
    plot = studyDesign(study_design) +
      theme(
        text = element_text(size = 14 * m)
      ) +
      labs(title = "Phase Lengths 3"),
    width = 6,
    height = 7,
    scale = m,
    dpi = 96 * m
  )
}
