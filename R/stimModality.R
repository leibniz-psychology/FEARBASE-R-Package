#' @title sex distribution
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

stimModality <- function(type = "us") {
  md <- getMetadata()

  if (
    tolower(type) %in%
      c("us", "unconditioned stimulus", "unconditioned stimuli")
  ) {
    graph <- md |>
      select(condition_id, study_id, us_type, cs_type) |>
      group_by(us_type) |>
      summarise(n = n()) |>
      arrange(n) |>
      mutate(us_type = factor(us_type, levels = us_type)) |>
      ggplot(aes(x = "", fill = us_type, y = n)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void(paper = "white") +
      geom_text(aes(label = n), position = position_stack(vjust = 0.5))
  } else if (
    tolower(type) %in% c("cs", "conditioned stimulus", "conditioned stimuli")
  ) {
    graph <- md |>
      select(condition_id, study_id, us_type, cs_type) |>
      group_by(cs_type) |>
      summarise(n = n()) |>
      arrange(n) |>
      mutate(cs_type = factor(cs_type, levels = cs_type)) |>
      ggplot(aes(x = "", fill = cs_type, y = n)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void(paper = "white") +
      geom_text(aes(label = n), position = position_stack(vjust = 0.5))
  } else {
    stop("unknown stimulus type")
  }

  return(graph)
}
