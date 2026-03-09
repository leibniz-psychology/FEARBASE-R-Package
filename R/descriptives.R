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

  sd <- get("metadata") |> # study data
    left_join(mapping, by = c("id" = "condition_id")) |>
    dplyr::rename("condition_id" = "id")

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
