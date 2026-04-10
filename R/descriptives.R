#' @title descriptives
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

descriptives <- function(dl = data_long, md = metadata) {
  unique(dl$measure)

  unique(dl[is.na(dl["measure"]), "study_id"])
  unique(dl["study_id"])
  # there are a many studies whrere the measure entry is missing, but the measurements are there

  participants <- length(unique(dl$participant_id))
  studies <- length(unique(dl$study_id))
  cond <- length(unique(dl$condition_id))

  names(md)
  dis <- length(unique(md$dataInstitution))
  dcs <- length(unique(md$dataCountry))
  lis <- length(unique(md$labInstitution))
  lcs <- length(unique(md$labCountry))
  # psych::describe(sd$year)

  return(c(
    "Studies" = studies,
    "Conditions" = cond,
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

  participants_with_extinction <- dl |>
    filter(phase == "ext") |>
    select(study_id, condition_id, participant_id) |>
    drop_na() |>
    distinct()

  participants_with_extinction |>
    left_join(
      # we capture the "time to next phase" so we need to look at the acquisition rows of study_design
      # only include conditions that have an extinction phase
      filter(sd, name == "acq", condition_id %in% studies_with_extinction),
      by = "condition_id"
    ) |>
    group_by(timeToNextUnit) |>
    summarise(n = n())
}
