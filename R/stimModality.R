#' Stimulus modalities
#'
#' @description
#' Generates a pie chart of the stimulus modalities.
#'
#' @param md The metadata.
#' @param type The type of stimulus to plot. Either "us_type" or "cs_type".
#' @param level The level of aggregation. Either "n_studies" or "n_subjects".
#'
#' @return A ggplot object.
#' @export
stimModality <- function(
  md = metadata,
  type = "us_type",
  level = "n_studies"
) {
  if (type != "us_type" & type != "cs_type") {
    stop("type must be either 'us_type' or 'cs_type'")
  }

  if (level != "n_studies" & level != "n_subjects") {
    stop("level must be either 'n_studies' or 'n_subjects'")
  }

  data <- md |>
    select(condition_id, study_id, n_subjects, us_type, cs_type) |>
    group_by(.data[[type]]) |>
    summarise(n_studies = n(), n_subjects = sum(n_subjects)) |>
    mutate(!!type := factor(.data[[type]], levels = .data[[type]]))

  title <- ifelse(type == "us_type", "US Modality", "CS Modality")

  graph <- data |>
    ggplot(aes(x = "", fill = .data[[type]], y = .data[[level]])) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void(paper = "white") +
    geom_label(
      aes(label = .data[[level]], group = .data[[type]]),
      fill = "white",
      position = position_stack(vjust = 0.5)
    ) +
    labs(fill = title)
  return(graph)
}
