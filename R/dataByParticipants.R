#' @title data collection year
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

dataByParticipants <- function() {
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
    geom_text(aes(label = n), vjust = -.5) +
    labs(x = "Study ID", y = "Number of Participants")

  return(graph)
}
