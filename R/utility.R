#' @title utility functions for FEARBASE package

getMetadata <- function() {
    url <- 'https://docs.google.com/spreadsheets/d/1INi9MHloIm8XtaNLOoj046xf-T1Afm3vqFI-zRGKONw/edit?gid=0#gid=0'
    mapping <- read.csv(text=gsheet::gsheet2text(url, format='csv'))

    if(!exists("metadata")) {
        load(file.path("data", "metadata.RData"))
    }
    output <- get("metadata") |>
        dplyr::left_join(mapping, by = c("id" = "condition_id")) |>
        dplyr::rename("condition_id" = "id")

    return(output)
}

getDataLong <- function() {
    url <- 'https://docs.google.com/spreadsheets/d/1INi9MHloIm8XtaNLOoj046xf-T1Afm3vqFI-zRGKONw/edit?gid=0#gid=0'
    mapping <- read.csv(text=gsheet::gsheet2text(url, format='csv'))

    if(!exists("data_long")) {
        load(file.path("data", "data_long.RData"))
    }

    output <- get("data_long") |>
        dplyr::left_join(mapping, by = c("study_id" = "condition_id"))|>
        dplyr::rename("condition_id" = "study_id", "study_id" = "study_id.y")

    return(output)
}
