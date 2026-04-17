library(dplyr)
library(readr)
library(tidyr)

args = commandArgs(trailingOnly = TRUE)

updateMapping <- function(path) {
  url <- 'https://docs.google.com/spreadsheets/d/1INi9MHloIm8XtaNLOoj046xf-T1Afm3vqFI-zRGKONw/edit?gid=0#gid=0'
  mapping <- read.csv(text = gsheet::gsheet2text(url, format = 'csv'))
  save(mapping, file = file.path(path, "data-preperation", "mapping.rda"))
  return(mapping)
}

csv_to_internal <- function(path) {
  mapping <- updateMapping(path)

  # read csv and give it the file's name
  files = list.files(
    file.path(path, "data-preperation", "input"),
    pattern = ".csv$"
  )
  for (f in files) {
    assign(
      sub(".csv", "", f),
      read_csv(file.path(path, "data-preperation", "input", f)),
      envir = .GlobalEnv
    )
  }

  data_long <- data_long |>
    left_join(mapping, by = c("study_id" = "condition_id")) |>
    rename("condition_id" = "study_id", "study_id" = "study_id.y")
  save(data_long, file = file.path(path, "data", "data_long.rda"))
  save(data_wide, file = file.path(path, "data", "data_wide.rda"))
  save(codebook, file = file.path(path, "data", "codebook.rda"))
  save(
    questionnaires,
    file = file.path(path, "data", "questionnaires.rda")
  )

  metadata <- metadata |>
    left_join(mapping, by = c("id" = "condition_id")) |>
    rename("condition_id" = "id")
  save(metadata, file = file.path(path, "data", "metadata.rda"))
  study_design <- study_design |>
    left_join(mapping, by = c("study_id" = "condition_id")) |>
    rename("condition_id" = "study_id", "study_id" = "study_id.y")
  save(
    study_design,
    file = file.path(path, "data", "study_design.rda")
  )

  #   data_long[data_long$study_id == "98" & data_long$phase == "hab", ]
  #   data_long |> filter(study_id == "98", phase == "hab", measure == "scr")
}

csv_to_internal(args[1])
