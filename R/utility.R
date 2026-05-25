#' All studies
#'
#' @description This function returns the list of all study IDs in the metadata.
#'
#' @param md The metadata.
#'
#' @return A character vector of all study IDs.
#' @export
allStudies <- function(md = metadata) {
  md <- .apply_mapping_to_metadata(md)

  studies <- md |>
    select(study_id) |>
    distinct() |>
    arrange(study_id) |>
    pull(study_id)

  return(studies)
}


#' @title Reorder phases
#' @description Returns a factor with standardized levels: priority phases first ("hab", "acq", "ext", "int", "rin", "rex", "rev", "other"), then others.
#' @param phases A vector of phases to be converted to factor levels.
#' @return A factor with the standardized phase levels.
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
