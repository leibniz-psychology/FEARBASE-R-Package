#' Deprecated camelCase compatibility wrappers
#'
#' @description
#' These functions preserve the package's historical camelCase API while the
#' preferred public API migrates to tidyverse-style snake_case names. New code
#' should call the snake_case function shown in each deprecation warning.
#'
#' @details
#' The wrappers are intentionally isolated in one file so the primary
#' implementation files can remain consistently snake_case. Each wrapper emits
#' a base R deprecation warning through `.Deprecated()` and then delegates to
#' the corresponding snake_case implementation without changing arguments or
#' return values.
#'
#' @param dl Long-format FEARBASE data.
#' @param md FEARBASE metadata.
#' @param cb FEARBASE codebook.
#' @param d Name of a caller-visible object.
#' @param envir Environment used to resolve `d`.
#' @param inherits Logical flag passed to object lookup.
#' @param warn_as_error Logical flag controlling warning escalation during
#'   object lookup.
#' @param file Path to a CSV file.
#' @param grouping_variable Grouping column name.
#' @param year_of Year selector for collection-year plots.
#' @param plot A ggplot object.
#' @param layer Layer index in `plot`.
#' @param data Data frame used by the selected plot layer.
#' @param row_id_col Name of the diagnostic row identifier column.
#' @param type Stimulus modality selector.
#' @param level Stimulus-modality aggregation level.
#' @param y_axis Trial-count y-axis selector.
#' @param exclude Phase codes to exclude from phase heatmaps.
#' @param assign_global Logical flag controlling whether mapping is assigned to
#'   the calling environment. Defaults to `FALSE`. When set to `TRUE`, this
#'   creates or updates `mapping` in the calling environment.
#'
#' @name deprecated-compatibility
NULL

#' @rdname deprecated-compatibility
#' @export
ageDescriptives <- function(dl, grouping_variable = NULL) {
  .Deprecated("age_descriptives")
  age_descriptives(dl = dl, grouping_variable = grouping_variable)
}

#' @rdname deprecated-compatibility
#' @export
allStudies <- function(md = NULL) {
  .Deprecated("all_studies")
  all_studies(md = md)
}

#' @rdname deprecated-compatibility
#' @export
checkData <- function(
  d,
  envir = parent.frame(),
  inherits = FALSE,
  warn_as_error = TRUE
) {
  .Deprecated("check_data")
  check_data(
    d = d,
    envir = envir,
    inherits = inherits,
    warn_as_error = warn_as_error
  )
}

#' @rdname deprecated-compatibility
#' @export
createCsv <- function(file) {
  .Deprecated("create_csv")
  create_csv(file = file)
}

#' @rdname deprecated-compatibility
#' @export
dataCollectionYear <- function(
  md = NULL,
  grouping_variable = "study_id",
  year_of = "publication"
) {
  .Deprecated("data_collection_year")
  data_collection_year(
    md = md,
    grouping_variable = grouping_variable,
    year_of = year_of
  )
}

#' @rdname deprecated-compatibility
#' @export
jsonSummary <- function(d) {
  .Deprecated("json_summary")
  json_summary(d = d, envir = parent.frame())
}

#' @rdname deprecated-compatibility
#' @export
measuresHeatmap <- function(dl, md, cb) {
  .Deprecated("measures_heatmap")
  measures_heatmap(dl = dl, md = md, cb = cb)
}

#' @rdname deprecated-compatibility
#' @export
peakDetectionWindows <- function(md, grouping_variable = "study_id") {
  .Deprecated("peak_detection_windows")
  peak_detection_windows(md = md, grouping_variable = grouping_variable)
}

#' @rdname deprecated-compatibility
#' @export
phasesHeatmap <- function(dl, cb, exclude = "none") {
  .Deprecated("phases_heatmap")
  phases_heatmap(dl = dl, cb = cb, exclude = exclude)
}

#' @rdname deprecated-compatibility
#' @export
reinforcementRates <- function(md = NULL, grouping_variable = "study_id") {
  .Deprecated("reinforcement_rates")
  reinforcement_rates(md = md, grouping_variable = grouping_variable)
}

#' @rdname deprecated-compatibility
#' @export
sampleSizeByStudy <- function(dl = NULL, grouping_variable = "study_id") {
  .Deprecated("sample_size_by_study")
  sample_size_by_study(dl = dl, grouping_variable = grouping_variable)
}

#' @rdname deprecated-compatibility
#' @export
stimModality <- function(
  md = NULL,
  type = "us_type",
  level = "n_studies"
) {
  .Deprecated("stimulus_modality")
  stimulus_modality(md = md, type = type, level = level)
}

#' @rdname deprecated-compatibility
#' @export
traceRemovedRows <- function(
  plot,
  layer = 1,
  data = plot$data,
  row_id_col = ".row_id"
) {
  .Deprecated("trace_removed_rows")
  trace_removed_rows(
    plot = plot,
    layer = layer,
    data = data,
    row_id_col = row_id_col
  )
}

#' @rdname deprecated-compatibility
#' @export
trialsPhaseParticipant <- function(
  dl = NULL,
  y_axis = "participants",
  grouping_variable = "condition_id",
  cb = NULL
) {
  .Deprecated("trial_phase_counts")
  trial_phase_counts(
    dl = dl,
    y_axis = y_axis,
    grouping_variable = grouping_variable,
    cb = cb
  )
}

#' @rdname deprecated-compatibility
#' @export
updateMapping <- function(assign_global = FALSE) {
  .Deprecated("update_mapping")
  update_mapping(assign_global = assign_global)
}
