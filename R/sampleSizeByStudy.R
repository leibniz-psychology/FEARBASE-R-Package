#' @title Study Sample Sizes
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

sampleSizeByStudy <- function() {
  dl <- getDataLong()

  graph <- dl |>
    select(study_id, participant_id) |>
    unique() |>
    group_by(study_id) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    mutate(study_id = factor(study_id, levels = study_id)) |>
    ggplot(aes(x = study_id, y = n)) +
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
