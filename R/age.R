#' @title sex distribution
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

age <- function(type = "histogram") {
  dl <- getDataLong()

  if (tolower(type) %in% c("histogram", "hist", "h")) {
    age <- dl |>
      filter(measure == "age") |>
      select(study_id, participant_id, value, measure) |>
      mutate(age = as.numeric(value), study_id = as.factor(study_id)) |>
      filter(!is.na(age)) |>
      group_by(age, study_id) |>
      summarise(n = n())

    study_order <- age |>
      group_by(study_id) |>
      summarise(mean_age = median(age)) |>
      arrange(desc(mean_age)) |>
      pull(study_id)

    age$study_id <- factor(age$study_id, levels = study_order)

    graph <- age |>
      ggplot(aes(x = age, y = n)) +
      geom_bar(
        aes(fill = study_id),
        stat = "identity",
        color = "black",
        linewidth = .2
      )
  } else if (tolower(type) %in% c("ridge", "density", "r", "d")) {
    age <- dl |>
      filter(measure == "age") |>
      select(study_id, participant_id, value, measure) |>
      mutate(age = as.numeric(value), study_id = as.factor(study_id)) |>
      filter(!is.na(age))

    study_order <- age |>
      group_by(study_id) |>
      summarise(mean_age = median(age)) |>
      arrange(desc(mean_age)) |>
      pull(study_id)

    age$study_id <- factor(age$study_id, levels = study_order)

    graph <- age |>
      ggplot(aes(x = age, y = study_id, group = study_id, fill = study_id)) +
      ggridges::geom_density_ridges()
  } else {
    stop("unknown argument type")
  }

  return(graph)
}
