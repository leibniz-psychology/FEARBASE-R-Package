#' Sex distribution
#'
#' @description
#' Generates a pie chart of the sex distribution.
#'
#' @param dl The data in long format.
#'
#' @return A ggplot object.
#' @export
sex <- function(dl = data_long) {
  sex <- dl |>
    select(study_id, participant_id, value, measure) |>
    filter(measure == "sex" | measure == "gender")

  sex <- dl |>
    filter(!(participant_id %in% sex$participant_id)) |>
    select(study_id, participant_id) |>
    distinct() |>
    mutate(value = "not reported", measure = "sex") |>
    bind_rows(sex)

  sex <- sex |>
    mutate(
      sex = factor(
        stringr::str_split_i(tolower(value), "", 1),
        levels = c("m", "f", "n"),
        labels = c("m", "f", "nr") # c("male", "female", "not reported")
      )
    )

  sex |>
    group_by(sex) |>
    summarise(n = n()) |>
    ggplot(aes(x = "", y = n, fill = sex)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    ggrepel::geom_label_repel(
      aes(label = paste0(sex, " (", n, ")"), group = sex),
      position = position_stack(vjust = 0.5),
      fill = "white"
    ) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}
