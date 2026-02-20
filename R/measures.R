#' @title measures
#' @description This function returns the measures available for a given study.
#' @param study_id The ID of the study for which to retrieve measures.
#' @return A list containing the measures available for the specified study.
#' @import dplyr
#' @export

measures <- function(study_id = NULL) {

    if (!is.null(study_id)) {
        input_id <- study_id
    } else {
        input_id <- allStudies()
    }

    dl <- getDataLong()

    measures <- dl |>
        filter(study_id %in% input_id) |>
        select(measure) |>
        drop_na() |>
        distinct() |>
        arrange(measure) |>
        pull(measure)

    return(measures)
}
