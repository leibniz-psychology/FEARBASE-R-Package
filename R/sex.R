#' Sex distribution
#'
#' @description
#' Generates a pie chart of the sex distribution.
#'
#' @param dl The data in long format.
#'
#' @return A ggplot object.
#' @export
sex <- function(dl) {
  dl <- .apply_mapping_to_long_data(dl)

  # Match the local preprocessing used to derive `data_sex`.
  data_sex <- dl |>
    select(study_id, participant_id, value, measure) |>
    filter(measure == "sex" | measure == "gender")

  data_sex <- dl |>
    filter(!(participant_id %in% data_sex$participant_id)) |>
    select(study_id, participant_id) |>
    distinct() |>
    mutate(value = "not reported", measure = "sex") |>
    bind_rows(data_sex)

  data_sex <- data_sex |>
    mutate(
      sex = factor(
        stringr::str_split_i(tolower(value), "", 1),
        levels = c("m", "f", "n"),
        labels = c("m", "f", "not reported")
      )
    )

  count_data <- data_sex |>
    group_by(sex) |>
    summarise(n = n(), .groups = "drop")

  label_data <- count_data |>
    filter(!is.na(sex))

  # Plot
  ggplot(count_data, aes(x = "", y = n, fill = sex)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(fill = "Sex") +
    theme_void() +
    geom_label(
      data = label_data,
      aes(label = paste0(sex, " (", n, ")"), group = sex),
      position = position_stack(vjust = 0.5),
      fill = "white"
    ) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}
