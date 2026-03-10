#' @title instruction graph
#' @import dplyr
#' @import ggplot2
#' @export

instructions <- function() {
  md <- getMetadata()

  graph <- md |>
    select(condition_id, study_id, starts_with("instruction")) |>
    group_by(study_id) |>
    distinct(instruction_contingency) |>
    group_by(instruction_contingency) |>
    summarise(n = n()) |>
    arrange(n) |>
    mutate(
      instruction_contingency = factor(
        instruction_contingency,
        levels = instruction_contingency
      )
    ) |>
    ggplot(aes(x = instruction_contingency, y = n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = "Contingency Instruction", y = "Number of Studies")

  return(graph)
}
