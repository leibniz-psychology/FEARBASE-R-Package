#' @title utility functions for FEARBASE package

updateMapping <- function() {
    url <- 'https://docs.google.com/spreadsheets/d/1INi9MHloIm8XtaNLOoj046xf-T1Afm3vqFI-zRGKONw/edit?gid=0#gid=0'
    mapping <- read.csv(text=gsheet::gsheet2text(url, format='csv'))
    saveRDS(mapping, file.path("data", "mapping.rds"))
}

getMetadata <- function() {
    if(!"mapping.rds" %in% list.files(file.path("data"))) {
        updateMapping()
    }

    if (!exists("mapping")) {
        mapping <- readRDS(file.path("data", "mapping.rds"))
    }

    if(!exists("metadata")) {
        metadata <- readRDS(file.path("data", "metadata.rds"))
    }

    output <- get("metadata") |>
        dplyr::left_join(mapping, by = c("id" = "condition_id")) |>
        dplyr::rename("condition_id" = "id")

    return(output)
}

getDataLong <- function() {
    if(!"mapping.rds" %in% list.files(file.path("data"))) {
        updateMapping()
    }
    
    if (!exists("mapping")) {
        mapping <- readRDS(file.path("data", "mapping.rds"))
    }

    if(!exists("data_long")) {
        data_long <- readRDS(file.path("data", "data_long.rds"))
    }

    output <- get("data_long") |>
        dplyr::left_join(mapping, by = c("study_id" = "condition_id"))|>
        dplyr::rename("condition_id" = "study_id", "study_id" = "study_id.y")

    return(output)
}
