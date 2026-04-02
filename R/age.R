#' @title sex distribution
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

age <- function(type = "histogram") {
  dl <- getDataLong()

  age <- dl |>
    filter(measure == "age") |>
    select(paper_id, participant_id, value, measure) |>
    mutate(age = as.numeric(value), paper_id = as.factor(paper_id)) |>
    filter(!is.na(age))

  study_order <- age |>
    group_by(paper_id) |>
    summarise(mean_age = median(age)) |>
    arrange(desc(mean_age)) |>
    pull(paper_id)

  if (tolower(type) %in% c("histogram", "hist", "h")) {
    age <- age |>
      group_by(age, paper_id) |>
      summarise(n = n())

    age$paper_id <- factor(age$paper_id, levels = study_order)

    graph <- age |>
      ggplot(aes(x = age, y = n)) +
      geom_bar(
        aes(fill = paper_id),
        stat = "identity",
        color = "white",
        linewidth = .2
      ) +
      labs(x = "Age", y = "Number of Participants", fill = "Study")
  } else if (tolower(type) %in% c("ridge", "density", "r", "d")) {
    age$paper_id <- factor(age$paper_id, levels = study_order)

    graph <- age |>
      ggplot(aes(x = age, y = paper_id, group = paper_id, fill = paper_id)) +
      ggridges::geom_density_ridges() +
      labs(x = "Age", y = "Study", fill = "Study") +
      theme(legend.position = "none")
  } else {
    stop("unknown argument type")
  }
  return(graph)
}

ageDescriptives <- function() {
  dl <- getDataLong()

  age <- dl |>
    filter(measure == "age") |>
    select(paper_id, participant_id, value, measure) |>
    mutate(age = as.numeric(value), paper_id = as.factor(paper_id)) |>
    filter(!is.na(age)) |>
    summarise(
      mean_age = mean(age),
      sd = sd(age),
      min = min(age),
      max = max(age)
    )

  return(age)
}
