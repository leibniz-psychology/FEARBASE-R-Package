#' @title Study Sample Sizes
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

sampleSizeByStudy <- function() {
  dl <- getDataLong()

  data <- dl |>
    select(paper_id, participant_id) |>
    unique() |>
    group_by(paper_id) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    mutate(paper_id = factor(paper_id, levels = paper_id))
  graph <- data |>
    ggplot(aes(x = paper_id, y = n)) +
    coord_flip(ylim = c(0, max(data$n) + 10)) +
    geom_bar(stat = "identity") +
    labs(x = "Study", y = "Number of Participants")

  return(graph)
}


sampleSizeDescriptives <- function() {
  dl <- getDataLong()

  dl |>
    select(paper_id, participant_id) |>
    unique() |>
    group_by(paper_id) |>
    summarise(n = n()) |>
    pull(n) |>
    psych::describe()
}
