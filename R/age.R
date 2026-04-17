#' Age distribution
#'
#' @param dl The data in long format.
#' @param type The type of plot to produce. Either "histogram", "hist", "h" or "ridge", "density", "r", "d".
#'
#' @return A ggplot object.
#' @export
age <- function(dl = data_long, type = "histogram") {
  age <- dl |>
    filter(measure == "age") |>
    select(paper_study_id, participant_id, value, measure) |>
    mutate(
      age = as.numeric(value),
      paper_study_id = as.factor(paper_study_id)
    ) |>
    filter(!is.na(age))

  study_order <- age |>
    group_by(paper_study_id) |>
    summarise(mean_age = median(age)) |>
    arrange(desc(mean_age)) |>
    pull(paper_study_id)

  if (tolower(type) %in% c("histogram", "hist", "h")) {
    age <- age |>
      group_by(age, paper_study_id) |>
      summarise(n = n())

    age$paper_study_id <- factor(age$paper_study_id, levels = study_order)

    graph <- age |>
      ggplot(aes(x = age, y = n)) +
      geom_bar(
        aes(fill = paper_study_id),
        stat = "identity",
        color = "white",
        linewidth = .2
      ) +
      labs(x = "Age", y = "Number of Participants", fill = "Study ID")
  } else if (tolower(type) %in% c("ridge", "density", "r", "d")) {
    age$paper_study_id <- factor(age$paper_study_id, levels = study_order)

    graph <- age |>
      ggplot(aes(
        x = age,
        y = paper_study_id,
        group = paper_study_id,
        fill = paper_study_id
      )) +
      ggridges::geom_density_ridges() +
      labs(x = "Age", y = "Study ID", fill = "Study ID") +
      theme(legend.position = "none")
  } else {
    stop("unknown argument type")
  }
  return(graph)
}

#' Age descriptives
#'
#' @param dl The data in long format.
#'
#' @return A data frame with mean, sd, min and max age.
#' @export
ageDescriptives <- function(dl = data_long) {
  age <- dl |>
    filter(measure == "age") |>
    select(paper_study_id, participant_id, value, measure) |>
    mutate(
      age = as.numeric(value),
      paper_study_id = as.factor(paper_study_id)
    ) |>
    filter(!is.na(age)) |>
    summarise(
      mean_age = mean(age),
      sd = sd(age),
      min = min(age),
      max = max(age)
    )

  return(age)
}
