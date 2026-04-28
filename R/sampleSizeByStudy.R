#' Study sample sizes
#'
#' @description
#' Generates a bar plot of the sample sizes for each study.
#'
#' @param dl The data in long format.
#' @param grouping_variable A string specifying the variable to group by (allowed values: "study_id" or "paper_study_id").
#'
#' @return A ggplot object.
#' @export
sampleSizeByStudy <- function(
  dat = data_sample_size,
  grouping_variable = "study_id"
) {
  graph <- dat |>
    ggplot(aes(
      x = forcats::fct_reorder(
        .data[[grouping_variable]],
        .x = `n`,
        .desc = TRUE
      ),
      y = n
    )) +
    coord_flip(ylim = c(0, max(dat$n) + 10)) +
    geom_bar(stat = "identity") +
    labs(x = "Study ID", y = "Number of Participants")

  return(graph)
}


sampleSizeDescriptives <- function(
  dl = data_long,
  grouping_variable = "study_id"
) {
  dl |>
    select(.data[[grouping_variable]], participant_id) |>
    unique() |>
    group_by(.data[[grouping_variable]]) |>
    summarise(n = n()) |>
    pull(n) |>
    psych::describe()
}
