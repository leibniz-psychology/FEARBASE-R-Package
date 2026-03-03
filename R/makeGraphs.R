makeGraphs <- function(folder = "output/") {
  ggsave(
    filename = file.path(folder, "age_r.png"),
    plot = age("r"),
    width = 10,
    height = 6
  )
  ggsave(
    filename = file.path(folder, "age_h.png"),
    plot = age("h"),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "measureHistogram.png"),
    plot = combinedHistogram(),
    width = 16,
    height = 10
  )

  ggsave(
    filename = file.path(folder, "dataByParticipants.png"),
    plot = dataByParticipants(),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "dataCollectionYear.png"),
    plot = dataCollectionYear(),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "dataCollectionYear.png"),
    plot = dataCollectionYear(),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "peakDetectionWindows.png"),
    plot = peakDetectionWindows(),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "phasesHistogram.png"),
    plot = phasesHistogram(),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "reinforcementRates.png"),
    plot = reinforcementRates(),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "sex.png"),
    plot = sex(),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "trialsPhaseParticipant_N.png"),
    plot = trialsPhaseParticipant("n"),
    width = 10,
    height = 6
  )
  ggsave(
    filename = file.path(folder, "trialsPhaseParticipant_S.png"),
    plot = trialsPhaseParticipant("s"),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "csModality.png"),
    plot = stimModality("cs"),
    width = 10,
    height = 6
  )
  ggsave(
    filename = file.path(folder, "usModality.png"),
    plot = stimModality("us"),
    width = 10,
    height = 6
  )

  ggsave(
    filename = file.path(folder, "instructions.png"),
    plot = instructions(),
    width = 10,
    height = 6
  )
}
