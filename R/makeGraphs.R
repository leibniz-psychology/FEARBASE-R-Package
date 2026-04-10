makeGraphs <- function(folder = "output/") {
  ggsave(
    filename = file.path(folder, "age_r.png"),
    plot = age(data_long, "r"),
    width = 10,
    height = 6
  )
  ggsave(
    filename = file.path(folder, "age_h.png"),
    plot = age(data_long, "h"),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "measureHeatmap.png"),
    plot = measuresHeatmap(),
    width = 16,
    height = 10
  )

  ggsave(
    filename = file.path(folder, "sampleSizeByStudy.png"),
    plot = sampleSizeByStudy(data_long),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "dataCollectionYear.png"),
    plot = dataCollectionYear(metadata),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "peakDetectionWindows.png"),
    plot = peakDetectionWindows(metadata),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "phasesHeatmap.png"),
    plot = phasesHeatmap(),
    width = 16,
    height = 10
  )

  ggsave(
    filename = file.path(folder, "reinforcementRates.png"),
    plot = reinforcementRates(metadata),
    width = 6,
    height = 4
  )

  ggsave(
    filename = file.path(folder, "sex.png"),
    plot = sex(data_long),
    width = 5,
    height = 5
  )

  ggsave(
    filename = file.path(folder, "trialsPhaseParticipant_N.png"),
    plot = trialsPhaseParticipant(data_long, "n"),
    width = 8,
    height = 8
  )
  ggsave(
    filename = file.path(folder, "trialsPhaseParticipant_S.png"),
    plot = trialsPhaseParticipant(data_long, "s"),
    width = 8,
    height = 8
  )

  ggsave(
    filename = file.path(folder, "csModalityParticipants.png"),
    plot = stimModality(metadata, type = "cs_type", level = "n_subjects"),
    width = 5,
    height = 5
  )
  ggsave(
    filename = file.path(folder, "usModalityParticipants.png"),
    plot = stimModality(metadata, type = "us_type", level = "n_subjects"),
    width = 5,
    height = 5
  )

  ggsave(
    filename = file.path(folder, "csModalityStudies.png"),
    plot = stimModality(metadata, type = "cs_type", level = "n_studies"),
    width = 5,
    height = 5
  )
  ggsave(
    filename = file.path(folder, "usModalityStudies.png"),
    plot = stimModality(metadata, type = "us_type", level = "n_studies"),
    width = 5,
    height = 5
  )

  ggsave(
    filename = file.path(folder, "instructions.png"),
    plot = instructions(metadata),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "phaseResponseDistribution_SCR.png"),
    plot = phaseResponseDistribution(data_long, "scr"),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "dataDensityMatrix.png"),
    plot = dataDensityMatrix(data_long),
    width = 16,
    height = 12
  )

  ggsave(
    filename = file.path(folder, "measureByStudy_All.png"),
    plot = measureByStudy(data_long),
    width = 16,
    height = 12
  )

  ggsave(
    filename = file.path(folder, "measureByStudy_Stimulus.png"),
    plot = measureByStudy(data_long, split_by_stimulus = TRUE),
    width = 16,
    height = 12
  )
}
