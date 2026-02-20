#' @title sex distribution
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

age <- function() {

    dl <- getDataLong()

    age <- dl |>
        filter(measure == "age") |>
        select(study_id, participant_id, value, measure) |>
        mutate(age = as.numeric(value),
            study_id = as.factor(study_id)) |>
        filter(!is.na(age)) |>
        group_by(age, study_id) |>
        summarise(n = n())

    graph <- age |>
        ggplot(aes(x = age, y = n)) + 
            geom_bar(aes(fill = study_id), stat = "identity")

    # age <- dl |>
    #     filter(measure == "age") |>
    #     select(study_id, participant_id, value, measure) |>
    #     mutate(age = as.numeric(value),
    #         study_id = as.factor(study_id)) 

    # graph <- age |>
    #     ggplot(aes(x = age)) +
    #         geom_density(aes(color = study_id, group = study_id)) +
    #         geom_density(color = "black", linewidth = 1.5)

    return(graph)
}
