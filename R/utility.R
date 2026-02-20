#' @title utility functions for FEARBASE package

updateMapping <- function() {
    url <- 'https://docs.google.com/spreadsheets/d/1INi9MHloIm8XtaNLOoj046xf-T1Afm3vqFI-zRGKONw/edit?gid=0#gid=0'
    mapping <- read.csv(text=gsheet::gsheet2text(url, format='csv'))
    usethis::use_data(mapping)
}

getMetadata <- function() {
    if(!"mapping.rda" %in% list.files(file.path("data"))) {
        updateMapping()
    }

    if (!exists("mapping")) {
        load(file.path("data", "mapping.rda"))
    }

    if(!exists("metadata")) {
       load(file.path("data", "metadata.rda"))
    }

    output <- get("metadata") |>
        dplyr::left_join(mapping, by = c("id" = "condition_id")) |>
        dplyr::rename("condition_id" = "id")

    return(output)
}

getDataLong <- function() {
    if(!"mapping.rda" %in% list.files(file.path("data"))) {
        updateMapping()
    }
    
    if (!exists("mapping")) {
        load(file.path("data", "mapping.rda"))
    }

    if(!exists("data_long")) {
        load(file.path("data", "data_long.rda"))
    }

    output <- get("data_long") |>
        dplyr::left_join(mapping, by = c("study_id" = "condition_id"))|>
        dplyr::rename("condition_id" = "study_id", "study_id" = "study_id.y")

    return(output)
}
