#' Study sample sizes
#'
#' @description
#' Generates a bar plot of the sample sizes for each study.
#'
#' @param dl The data in long format.
#'
#' @return A ggplot object.
#' @export
sampleSizeByStudy <- function(dl) {
  # Process data
  data_sample_size <- dl |>
    select(study_id, participant_id) |>
    unique() |>
    group_by(study_id) |>
    summarise(n = n()) |>
    arrange(desc(n)) |>
    mutate(across(study_id, as.factor)
  )

  # Plot
  graph <- dat |>
    ggplot(aes(
      x = forcats::fct_reorder(
        .data[['study_id']],
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
