#' Study sample sizes
#'
#' @description
#' Generates a bar plot of the sample sizes for each study.
#'
#' @param dl The data in long format.
#' @param grouping_variable A string specifying the variable to group by (allowed
#'   values: "study_id" or "paper_study_id").
#'
#' @return A ggplot object.
#' @export
sampleSizeByStudy <- function(
  dl,
  grouping_variable = "study_id"
) {
  dl <- .apply_mapping_to_long_data(dl)

  # Process data
  data_sample_size <- dl |>
    dplyr::select(dplyr::all_of(c(grouping_variable, "participant_id"))) |>
    unique() |>
    dplyr::group_by(.data[[grouping_variable]]) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::mutate(
      !!grouping_variable := as.factor(.data[[grouping_variable]])
    )

  # Plot
  graph <- data_sample_size |>
    ggplot2::ggplot(ggplot2::aes(
      x = forcats::fct_reorder(
        .data[[grouping_variable]],
        .x = `n`,
        .desc = TRUE
      ),
      y = n
    )) +
    ggplot2::coord_flip(ylim = c(0, max(data_sample_size$n) + 10)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Study ID", y = "Number of Participants")

  return(graph)
}


sampleSizeDescriptives <- function(
  dl = data_long,
  grouping_variable = "study_id"
) {
  dl <- .apply_mapping_to_long_data(dl)

  dl |>
    dplyr::select(dplyr::all_of(c(grouping_variable, "participant_id"))) |>
    unique() |>
    dplyr::group_by(.data[[grouping_variable]]) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull("n") |>
    psych::describe()
}
