#' Instruction graph
#'
#' @description
#' Generates a bar plot of the contingency instructions.
#'
#' @param md The metadata.
#'
#' @return A ggplot object.
#' @export
instructions <- function(md = metadata) {
  graph <- data |>
    ggplot(aes(x = instruction_contingency, y = n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = "Contingency Instruction", y = "Number of Studies")

  return(graph)
}

makeInstructionsDetails <- function(folder = "output/") {
  md |>
    select(condition_id, study_id, starts_with("instruction")) |>
    drop_na(instruction_contingency_details) |>
    write_csv(file.path(folder, "instructions.csv"))
}
