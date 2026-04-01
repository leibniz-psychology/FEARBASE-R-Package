#' @title Study Sample Sizes
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

sampleSizeByStudy <- function() {
  dl <- getDataLong()

  data <- dl |>
    select(study_id, participant_id) |>
    unique() |>
    group_by(study_id) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    mutate(study_id = factor(study_id, levels = study_id))
  graph <- data |>
    ggplot(aes(x = study_id, y = n)) +
    coord_flip(ylim = c(0, max(data$n) + 10)) +
    geom_bar(stat = "identity") +
    labs(x = "Study ID", y = "Number of Participants")

  return(graph)
}


sampleSizeDescriptives <- function() {
  dl <- getDataLong()

  dl |>
    select(study_id, participant_id) |>
    unique() |>
    group_by(study_id) |>
    summarise(n = n()) |>
    pull(n) |>
    psych::describe()
}
