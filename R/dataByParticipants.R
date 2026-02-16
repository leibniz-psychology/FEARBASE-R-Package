#' @title data collection year
#' @export

dataCollectionYear <- function() {
    library(dplyr)
    library(ggplot2)
    library(tidyr)

    dl <- getDataLong()

    graph <- dl |>
        select(study_id, participant_id) |>
        unique() |>
        group_by(study_id) |>
        summarise(n = n()) |>
        arrange(desc(n)) |>
        mutate(study_id = factor(study_id, levels = study_id)) |>
        ggplot(aes(x = study_id, y=n)) +
        geom_bar(stat = "identity") +
        labs(x = "Study ID", y = "Number of Participants")

    return(graph)
}