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

set_theme(theme_classic())

# update_theme(palette.colour.discrete = grDevices::colorRampPalette(c("#000000", "#0032A0", "#ffffff")))
update_theme(
  palette.colour.discrete = grDevices::colorRampPalette(c(
    "#ffece2",
    "#ff8800",
    "#0032A0"
  ))
)
update_geom_defaults("bar", list("fill" = "#0032A0"))

# grDevices::colorRampPalette(c("#ffb184", "#0032A0", "#3f003f"))(30) |>
# Polychrome::swatch()
