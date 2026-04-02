#' @title descriptives
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

descriptives <- function() {
  dl <- getDataLong()

  unique(dl$measure)

  unique(dl[is.na(dl["measure"]), "study_id"])
  unique(dl["study_id"])
  # there are a many studies whrere the measure entry is missing, but the measurements are there

  participants <- length(unique(dl$participant_id))
  studies <- length(unique(dl$study_id))

  sd <- getMetadata()

  names(sd)
  dis <- length(unique(sd$dataInstitution))
  dcs <- length(unique(sd$dataCountry))
  lis <- length(unique(sd$labInstitution))
  lcs <- length(unique(sd$labCountry))
  # psych::describe(sd$year)

  return(c(
    "Studies" = studies,
    "Participants" = participants,
    "Data Collection Institutions" = dis,
    "Data Collection Countries" = dcs,
    "Data Analysis Labs" = lis,
    "Data Analysis Lab Countries" = lcs
  ))
}

participants <- function(dl) {
  dl |>
    select(study_id, condition_id, participant_id) |>
    distinct()
}

delayedExtinction <- function(sd, dl) {
  studies_with_extinction <- sd |>
    filter(name == "ext") |>
    pull(condition_id)

  participants <- dl |>
    filter(phase == "ext") |>
    select(study_id, condition_id, participant_id) |>
    drop_na() |>
    distinct()

  sd |>
    filter(name == "acq", condition_id %in% studies_with_extinction) |>
    print(n = Inf) |>
    left_join(participants, by = "condition_id") |>
    group_by(timeToNextUnit) |>
    summarise(n = n())
}
