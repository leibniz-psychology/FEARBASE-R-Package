#' @title utility functions for FEARBASE package

updateMapping <- function() {
  url <- 'https://docs.google.com/spreadsheets/d/1INi9MHloIm8XtaNLOoj046xf-T1Afm3vqFI-zRGKONw/edit?gid=0#gid=0'
  mapping <- read.csv(text = gsheet::gsheet2text(url, format = 'csv'))
  usethis::use_data(mapping)
}

getMetadata <- function() {
  output <- get("metadata") |>
    dplyr::left_join(mapping, by = c("id" = "condition_id")) |>
    dplyr::rename("condition_id" = "id")

  return(output)
}

getDataLong <- function() {
  output <- get("data_long") |>
    dplyr::left_join(mapping, by = c("study_id" = "condition_id")) |>
    dplyr::rename("condition_id" = "study_id", "study_id" = "study_id.y")

  return(output)
}

#' @title All Studies
#' @description This function returns the list of all study IDs in the metadata.
#' @return A list containing all study IDs.
#' @import dplyr
#' @export
allStudies <- function() {
  md <- getMetadata()

  studies <- md |>
    select(study_id) |>
    distinct() |>
    arrange(study_id) |>
    pull(study_id)

  return(studies)
}

#' @import readr
#' @import dplyr
csv_to_internal <- function(files = list.files("data", pattern = ".csv$")) {
  for (f in files) {
    assign(sub(".csv", "", f), read_csv(file.path("data", f)))
  }

  usethis::use_data(data_long, overwrite = TRUE)
  usethis::use_data(data_wide, overwrite = TRUE)
  usethis::use_data(codebook, overwrite = TRUE)
  usethis::use_data(questionnaires, overwrite = TRUE)
  usethis::use_data(metadata, overwrite = TRUE)
  usethis::use_data(study_design, overwrite = TRUE)

  #   data_long[data_long$study_id == "98" & data_long$phase == "hab", ]
  #   data_long |> filter(study_id == "98", phase == "hab", measure == "scr")
}
