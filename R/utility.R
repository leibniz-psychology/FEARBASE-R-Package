#' @title utility functions for FEARBASE package

updateMapping <- function() {
  url <- 'https://docs.google.com/spreadsheets/d/1INi9MHloIm8XtaNLOoj046xf-T1Afm3vqFI-zRGKONw/edit?gid=0#gid=0'
  mapping <- read.csv(text = gsheet::gsheet2text(url, format = 'csv'))
  usethis::use_data(mapping, overwrite = TRUE)
  return(mapping)
}

#' @title All Studies
#' @description This function returns the list of all study IDs in the metadata.
#' @return A list containing all study IDs.
#' @import dplyr
#' @export
allStudies <- function(md = metadata) {
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
  mapping <- updateMapping()

  # read csv and give it the file's name
  for (f in files) {
    assign(sub(".csv", "", f), read_csv(file.path("data", f)))
  }

  data_long <- data_long |>
    left_join(mapping, by = c("study_id" = "condition_id")) |>
    rename("condition_id" = "study_id", "study_id" = "study_id.y")
  use_data(data_long, overwrite = TRUE)
  use_data(data_wide, overwrite = TRUE)
  use_data(codebook, overwrite = TRUE)
  use_data(questionnaires, overwrite = TRUE)

  metadata <- metadata |>
    left_join(mapping, by = c("id" = "condition_id")) |>
    rename("condition_id" = "id")
  use_data(metadata, overwrite = TRUE)
  study_design <- study_design |>
    left_join(mapping, by = c("study_id" = "condition_id")) |>
    rename("condition_id" = "study_id", "study_id" = "study_id.y")
  use_data(study_design, overwrite = TRUE)

  #   data_long[data_long$study_id == "98" & data_long$phase == "hab", ]
  #   data_long |> filter(study_id == "98", phase == "hab", measure == "scr")
}

#' @title Reorder phases
#' @description Returns a factor with standardized levels: priority phases first ("hab", "acq", "ext", "int", "rin", "rex", "rev", "other"), then others.
#' @param phases A vector of phases to be converted to factor levels.
#' @return A factor with the standardized phase levels.
#' @import dplyr
#' @import forcats
#' @export
reorderPhases <- function(phases) {
  unique_phases <- unique(as.character(phases))
  priority_phases <- c("hab", "acq", "ext", "int", "rin", "rex", "rev", "other")
  existing_priority <- priority_phases[priority_phases %in% unique_phases]
  other_phases <- setdiff(unique_phases, priority_phases)
  phase_levels <- c(existing_priority, other_phases)

  return(factor(phases, levels = phase_levels))
}

# taken from stack overflow, see https://stackoverflow.com/questions/28562288/how-to-use-the-hsl-hue-saturation-lightness-cylindric-color-model
hsl_to_rgb <- function(h, s, l) {
  h <- h / 360
  r <- g <- b <- 0.0
  if (s == 0) {
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) {
        t <- t + 1.0
      }
      if (t > 1) {
        t <- t - 1.0
      }
      if (t < 1 / 6) {
        return(p + (q - p) * 6.0 * t)
      }
      if (t < 1 / 2) {
        return(q)
      }
      if (t < 2 / 3) {
        return(p + ((q - p) * ((2 / 3) - t) * 6))
      }
      return(p)
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l * s))
    p <- 2.0 * l - q
    r <- hue_to_rgb(p, q, h + 1 / 3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1 / 3)
  }
  return(rgb(r, g, b))
}
