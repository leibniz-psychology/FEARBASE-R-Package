#' Instruction graph
#'
#' @description
#' Generates a bar plot of the contingency instructions.
#'
#' @param md The metadata.
#'
#' @return A ggplot object.
#' @export
instructions <- function(md) {
  md <- .apply_mapping_to_metadata(md)

  # Process data
  data_instructions <- md |>
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
  )

  # Plot
  graph <- data_instructions |>
    ggplot(aes(x = instruction_contingency, y = n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = "Contingency Instruction", y = "Number of Studies")

  return(graph)
}

makeInstructionsDetails <- function(folder = "output/") {
  md |>
    .apply_mapping_to_metadata() |>
    select(study_id, starts_with("instruction")) |>
    drop_na(instruction_contingency_details) |>
    write_csv(file.path(folder, "instructions.csv"))
}
