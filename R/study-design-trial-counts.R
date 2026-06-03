#' Resolve Study-Design Data for Trial-Phase Helpers
#'
#' @param sd A study-design data frame supplied by the caller, or `NULL`.
#' @param caller_env Environment to inspect for an object named `study_design`.
#'
#' @return A study-design data frame.
#' @noRd
.resolve_trials_phase_study_design <- function(
  sd = NULL,
  caller_env = parent.frame()
) {
  .resolve_caller_data(
    data = sd,
    arg_name = "sd",
    object_name = "study_design",
    caller_env = caller_env
  )
}


#' Plot Study-Design Trial Counts per Phase
#'
#' @description
#' Creates a faceted bar plot of phase-level trial counts from the study-design
#' table, where phases are labeled dynamically from the FEARBASE codebook.
#'
#' @param sd A study-design data frame, or `NULL`. Must contain `study_id`,
#'   `name`, `cspTrials`, and `csmTrials` after
#'   `.apply_mapping_to_study_design()` is applied. The `name` column contains
#'   phase abbreviations. If `NULL`, the function first attempts to use an
#'   object named `study_design` from the calling environment.
#' @param cb A codebook data frame, or `NULL`. If `NULL`, the function first
#'   attempts to use an object named `codebook` from the calling environment.
#'
#' @return A `ggplot2` object with one facet row per phase.
#'
#' @noRd
study_design_trial_counts <- function(sd = NULL, cb = NULL) {
  ############################################################
  # 1) Resolve, map, and validate study-design inputs
  ############################################################

  # Capture the unevaluated expression so legacy calls such as
  # study_design_trial_counts(study_design) can fall back to bundled data when
  # `study_design` is not an object in the current test or interactive
  # environment.
  sd_expr <- substitute(sd)
  sd <- tryCatch(
    sd,
    error = function(error) {
      if (identical(as.character(sd_expr), "study_design")) {
        return(NULL)
      }

      stop(error)
    }
  )

  sd <- .resolve_trials_phase_study_design(
    sd = sd,
    caller_env = parent.frame()
  )
  cb <- .resolve_codebook(cb, caller_env = parent.frame())

  .validate_data_frame(sd, "sd")
  .validate_data_frame(cb, "cb")

  # Study-design tables use `name` for the phase abbreviation. Apply the
  # package mapping first so `study_id` has the same meaning as in long data.
  sd <- .apply_mapping_to_study_design(sd)

  .validate_required_columns(
    sd,
    c("study_id", "name", "cspTrials", "csmTrials"),
    "sd"
  )

  ############################################################
  # 2) Prepare phase-level trial counts from the design table
  ############################################################

  # Keep rows with complete CS+ and CS- trial counts, exclude non-plotted phase
  # categories, and de-duplicate design rows before aggregation.
  trials <- sd |>
    filter(
      !is.na(.data$cspTrials),
      !is.na(.data$csmTrials),
      !is.na(.data$name),
      !.data$name %in% c("int", "other")
    ) |>
    distinct() |>
    group_by(
      .data$study_id,
      .data$name
    ) |>
    summarise(
      trials = sum(.data$cspTrials),
      .groups = "drop"
    ) |>
    mutate(
      name = .label_phases_from_codebook(
        .data$name,
        cb = cb,
        keep_unmapped = FALSE
      )
    ) |>
    filter(!is.na(.data$name))

  trials$study_id <- as.factor(trials$study_id)

  if (nrow(trials) == 0L) {
    stop(
      "No mapped study-design trial counts were available after excluding ",
      "intervention and other phases.",
      call. = FALSE
    )
  }

  ############################################################
  # 3) Count studies per phase-level trial count and plot
  ############################################################

  graph <- trials |>
    distinct(
      .data$study_id,
      .data$name,
      .data$trials
    ) |>
    group_by(
      .data$name,
      .data$trials
    ) |>
    summarise(
      n = n(),
      .groups = "drop"
    ) |>
    ggplot(
      aes(
        x = .data$trials,
        y = .data$n
      )
    ) +
    geom_col() +
    facet_grid(
      rows = vars(.data$name),
      axes = "all",
      axis.labels = "all_x"
    ) +
    scale_x_continuous(breaks = scales::extended_breaks(10)) +
    labs(x = "Number of Trials", y = "Number of Studies")

  return(graph)
}
