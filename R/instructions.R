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
        ggplot(aes(x = instruction_contingency)) +
            geom_bar() +
            coord_flip()
            
    return(graph)
}
