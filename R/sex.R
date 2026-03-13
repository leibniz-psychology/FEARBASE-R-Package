#' @title sex distribution
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

sex <- function() {
  dl <- getDataLong()

  sex <- dl |>
    filter(measure == "sex" | measure == "gender") |>
    select(study_id, participant_id, value, measure)

  sex <- sex |>
    mutate(
      sex = factor(
        stringr::str_split_i(tolower(value), "", 1),
        levels = c("m", "f"),
        labels = c("male", "female")
      )
    )

  sex |>
    group_by(sex) |>
    summarise(n = n())

  sex |>
    group_by(sex) |>
    summarise(n = n()) |>
    ggplot(aes(x = "", y = n, fill = sex)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void(paper = "white") +
    geom_label(
      aes(label = paste0(sex, " (", n, ")"), group = sex),
      position = position_stack(vjust = 0.5),
      fill = "white"
    ) +
    theme(legend.position = "none")
}
