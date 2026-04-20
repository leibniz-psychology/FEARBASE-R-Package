#' Study sample sizes
#'
#' @description
#' Generates a bar plot of the sample sizes for each study.
#'
#' @param dl The data in long format.
#'
#' @return A ggplot object.
#' @export
sampleSizeByStudy <- function(dat = data_sample_size) {
  graph <- dat |>
    ggplot(aes(x = paper_study_id, y = n)) +
    coord_flip(ylim = c(0, max(dat$n) + 10)) +
    geom_bar(stat = "identity") +
    labs(x = "Study ID", y = "Number of Participants")

  return(graph)
}


sampleSizeDescriptives <- function(dl = data_long) {
  dl |>
    select(paper_study_id, participant_id) |>
    unique() |>
    group_by(paper_study_id) |>
    summarise(n = n()) |>
    pull(n) |>
    psych::describe()
}
