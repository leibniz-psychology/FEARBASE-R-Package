#' @title distribution of a measure across phases
#' @description
#' Visualizes the distribution of a specific measure's values across different experimental phases.
#' @param measure_name The name of the measure to visualize (e.g., "scr", "expect", "fear")
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export
phaseResponseDistribution <- function(measure_name = "scr") {
  dl <- getDataLong()

  plot_data <- dl |>
    filter(measure == measure_name) |>
    filter(!is.na(phase), !is.na(value)) |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value))

  if (nrow(plot_data) == 0) {
    stop(paste("No data found for measure:", measure_name))
  }

  ggplot(plot_data, aes(x = phase, y = value, fill = phase)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +
    theme_minimal() +
    labs(
      title = paste("Distribution of", measure_name, "across Phases"),
      x = "Phase",
      y = "Value"
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#' @title global data density matrix
#' @description
#' Visualizes the availability of different measures across all studies.
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export
dataDensityMatrix <- function() {
  dl <- getDataLong()

  # Calculate percentage of participants per study/measure
  density_data <- dl |>
    group_by(study_id) |>
    mutate(total_p = n_distinct(participant_id)) |>
    group_by(study_id, measure, total_p) |>
    summarise(p_with_data = n_distinct(participant_id), .groups = "drop") |>
    mutate(percentage = (p_with_data / total_p) * 100) |>
    filter(!is.na(measure))

  ggplot(
    density_data,
    aes(x = measure, y = as.factor(study_id), fill = percentage)
  ) +
    geom_tile() +
    scale_fill_viridis_c(name = "% Participants") +
    theme_minimal() +
    labs(
      title = "Data Availability Matrix (Studies vs. Measures)",
      x = "Measure",
      y = "Study ID"
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

#' @title distribution of a measure across studies
#' @description
#' Compare the distribution of a questionnaire or rating across different studies.
#' @param measure_name The name of the measure (e.g., "stais", "age")
#' @import dplyr
#' @import ggplot2
#' @export
measureByStudy <- function(measure_name = "stais") {
  dl <- getDataLong()

  plot_data <- dl |>
    filter(measure == measure_name) |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value))

  if (nrow(plot_data) == 0) {
    stop(paste("No data found for measure:", measure_name))
  }

  ggplot(
    plot_data,
    aes(x = as.factor(study_id), y = value, fill = as.factor(study_id))
  ) +
    geom_boxplot() +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Distribution of", measure_name, "by Study"),
      x = "Study ID",
      y = "Value"
    ) +
    theme(legend.position = "none")
}
