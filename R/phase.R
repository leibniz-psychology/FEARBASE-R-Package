#' @title Reorder phases
#' @description Returns a factor with standardized levels: priority phases first ("hab", "acq", "ext", "int", "rin", "rex", "rev", "other"), then others.
#' @param phases A vector of phases to be converted to factor levels.
#' @param order Character vector of priority phase levels.
#' @return A factor with the standardized phase levels.
#' @noRd
reorder_phases <- function(phases, order) {
  unique_phases <- unique(as.character(phases))
  priority_phases <- order
  existing_priority <- priority_phases[priority_phases %in% unique_phases]
  other_phases <- setdiff(unique_phases, priority_phases)
  phase_levels <- c(existing_priority, other_phases)

  return(factor(phases, levels = phase_levels))
}

#' Build Phase Display Labels from the Codebook
#'
#' @param phases A vector containing FEARBASE phase abbreviations.
#' @param cb A codebook data frame.
#' @param defined_order Optional character vector defining priority display
#'   labels. Values not present in the data are ignored, and additional labels
#'   are appended after the priority labels.
#' @param keep_unmapped Logical. If `TRUE`, phase codes absent from the
#'   codebook are retained as display values instead of becoming `NA`.
#'
#' @return A factor with codebook-derived display labels.
#' @noRd
.label_phases_from_codebook <- function(
  phases,
  cb,
  defined_order = NULL,
  keep_unmapped = FALSE
) {
  ############################################################
  # 1) Build the abbreviation-to-label lookup
  ############################################################

  # Phase names are user-facing plot labels, so the codebook `name` field is
  # the single source of truth instead of hard-coded recode tables.
  phase_mapping <- .get_codebook_label_mapping(
    cb = cb,
    attribute = "phase",
    value_col = "phase_short",
    label_col = "phase_long",
    title_case = TRUE
  )

  ############################################################
  # 2) Translate codes while preserving vector length and row order
  ############################################################

  phase_values <- as.character(phases)
  phase_labels <- phase_mapping$phase_long[
    match(phase_values, phase_mapping$phase_short)
  ]

  # Keeping unmapped labels is useful for exploratory plotting because newly
  # introduced phase codes remain visible until the codebook is updated.
  if (isTRUE(keep_unmapped)) {
    phase_labels <- dplyr::coalesce(phase_labels, phase_values)
  }

  ############################################################
  # 3) Apply a stable factor order for plotting
  ############################################################

  # Default to the core fear-conditioning order and append later phases from
  # the codebook after that sequence. Callers may supply a narrower priority
  # order when a plot intentionally highlights only a subset first.
  if (is.null(defined_order)) {
    defined_order <- .label_phases_from_codebook(
      phases = c("hab", "acq", "ext", "rin", "rex", "rev"),
      cb = cb,
      defined_order = character(0),
      keep_unmapped = FALSE
    )
    defined_order <- as.character(defined_order)
  }

  reorder_phases(phase_labels, defined_order)
}
