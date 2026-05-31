#' All studies
#'
#' @description This function returns the list of all study IDs in the metadata.
#'
#' @param md The metadata. If `NULL`, an object named `metadata` is looked up
#'   in the calling environment.
#'
#' @return A character vector of all study IDs.
#' @export
all_studies <- function(md = NULL) {
  md <- .resolve_metadata(md, caller_env = parent.frame())
  md <- .apply_mapping_to_metadata(md)

  studies <- md |>
    select(all_of("study_id")) |>
    distinct() |>
    arrange(.data$study_id) |>
    pull(all_of("study_id"))

  return(studies)
}
