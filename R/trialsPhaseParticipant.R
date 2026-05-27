#' Validate Trial-Phase Grouping Input
#'
#' @param dl A mapped long-format data frame.
#' @param grouping_variable A single grouping column name.
#'
#' @return Invisibly returns `grouping_variable` when validation succeeds.
#' @noRd
.validate_trials_phase_grouping <- function(dl, grouping_variable) {
  ############################################################
  # 1) Validate the grouping declaration
  ############################################################

  # The trial-count summaries aggregate by exactly one identifier column. A
  # scalar character value keeps the later `.data[[grouping_variable]]` calls
  # unambiguous and avoids non-standard-evaluation surprises.
  .validate_single_column_name(grouping_variable, "grouping_variable")

  # Keep the public contract aligned with the identifiers produced by the
  # package mapping helpers. Rejecting unsupported names early gives clearer
  # feedback than a later missing-column error from dplyr.
  valid_group_vars <- c(
    "condition_id",
    "study_id",
    "paper_cond_id",
    "paper_study_id"
  )

  if (!grouping_variable %in% valid_group_vars) {
    stop(
      "`grouping_variable` must be one of: ",
      paste(valid_group_vars, collapse = ", "),
      call. = FALSE
    )
  }

  ############################################################
  # 2) Validate the mapped data schema
  ############################################################

  # These columns define one trial observation within one participant, phase,
  # and stimulus stream. All are needed before trial counts can be computed.
  .validate_required_columns(
    dl,
    c(grouping_variable, "participant_id", "phase", "stimulus", "trial"),
    "dl"
  )

  invisible(grouping_variable)
}

#' Validate Trial-Phase Y-Axis Input
#'
#' @param y_axis A scalar character y-axis selector.
#'
#' @return The normalized y-axis mode, either `"participants"` or `"studies"`.
#' @noRd
.validate_trials_phase_y_axis <- function(y_axis) {
  ############################################################
  # 1) Validate the raw selector
  ############################################################

  .validate_single_column_name(y_axis, "y_axis")

  ############################################################
  # 2) Normalize supported aliases to internal mode names
  ############################################################

  y_axis <- tolower(y_axis)

  if (y_axis %in% c("n", "participant", "participants")) {
    return("participants")
  }

  if (y_axis %in% c("s", "study", "studies")) {
    return("studies")
  }

  stop(
    "`y_axis` must be one of: n, participant, participants, s, study, ",
    "or studies.",
    call. = FALSE
  )
}

#' Add Supported Grouping Columns from the Mapping Table When Needed
#'
#' @param dl A mapped long-format data frame.
#' @param grouping_variable A single grouping column name.
#'
#' @return `dl`, optionally joined with the requested grouping column.
#' @noRd
.augment_trials_phase_grouping <- function(dl, grouping_variable) {
  ############################################################
  # 1) Return immediately when the requested grouping already exists
  ############################################################

  # Some input data already include paper-level identifiers. In that common
  # case, avoid an unnecessary join and preserve the caller's existing column.
  if (grouping_variable %in% names(dl)) {
    return(dl)
  }

  ############################################################
  # 2) Recover optional identifiers from the package mapping table
  ############################################################

  # .apply_mapping_to_long_data() is intentionally idempotent when
  # `condition_id` is already present, which means optional mapping columns such
  # as `paper_study_id` may still be absent. Join them only when the mapping can
  # provide the requested supported grouping variable.
  mapping <- .get_mapping()

  if (
    "condition_id" %in% names(dl) &&
      all(c("condition_id", grouping_variable) %in% names(mapping))
  ) {
    grouping_lookup <- mapping |>
      select(
        "condition_id",
        all_of(grouping_variable)
      ) |>
      distinct()

    return(
      dl |>
        left_join(grouping_lookup, by = "condition_id")
    )
  }

  return(dl)
}

#' Prepare Trial Counts per Participant and Phase
#'
#' @param dl A data frame in FEARBASE long format, or `NULL`.
#' @param cb A codebook data frame, or `NULL`.
#' @param grouping_variable A single grouping column name.
#' @param caller_env Environment used to resolve omitted data objects.
#'
#' @return A tibble with one row per grouping value, participant, phase, and
#'   phase-level trial count.
#' @noRd
.prepare_trials_phase_participant_data <- function(
  dl = NULL,
  cb = NULL,
  grouping_variable = "condition_id",
  caller_env = parent.frame()
) {
  ############################################################
  # 1) Resolve and validate external data sources
  ############################################################

  # Reuse the package-wide long-data resolver so zero-argument calls behave the
  # same way as the other participant-level visualization helpers.
  dl <- .resolve_sample_size_long_data(dl)
  cb <- .resolve_codebook(cb, caller_env = caller_env)

  # Tidyverse pipelines and mapping helpers require rectangular data with
  # stable column names. Validate before any schema transformation.
  .validate_data_frame(dl, "dl")
  .validate_data_frame(cb, "cb")

  ############################################################
  # 2) Normalize identifiers and validate the analysis columns
  ############################################################

  # Apply the package mapping before checking grouping columns because the
  # mapping step can create `condition_id`, `study_id`, and paper identifiers.
  dl <- .apply_mapping_to_long_data(dl)
  dl <- .augment_trials_phase_grouping(dl, grouping_variable)

  .validate_trials_phase_grouping(dl, grouping_variable)

  ############################################################
  # 3) Count trials for each participant within phase and stimulus streams
  ############################################################

  # Work from only the required columns, remove incomplete trial observations,
  # and de-duplicate long-format rows so repeated measurements do not inflate
  # the participant-level trial counts.
  participant_stimulus_trials <- dl |>
    select(
      all_of(c(
        grouping_variable,
        "participant_id",
        "phase",
        "stimulus",
        "trial"
      ))
    ) |>
    filter(
      !is.na(.data[[grouping_variable]]),
      !is.na(.data$participant_id),
      !is.na(.data$phase),
      !is.na(.data$stimulus),
      !is.na(.data$trial),
      !.data$phase %in% c("int", "other")
    ) |>
    distinct() |>
    group_by(
      .data[[grouping_variable]],
      .data$participant_id,
      .data$phase,
      .data$stimulus
    ) |>
    summarise(
      stimulus_trials = max(.data$trial),
      .groups = "drop"
    )

  ############################################################
  # 4) Collapse stimulus streams to phase-level participant totals
  ############################################################

  # A participant may have separate rows for multiple stimuli within the same
  # phase. Summing the stimulus-specific maxima gives the participant's total
  # number of recorded trials for that phase.
  participant_phase_trials <- participant_stimulus_trials |>
    group_by(
      .data[[grouping_variable]],
      .data$participant_id,
      .data$phase
    ) |>
    summarise(
      trials = sum(.data$stimulus_trials),
      .groups = "drop"
    ) |>
    mutate(
      phase = .label_phases_from_codebook(
        .data$phase,
        cb = cb,
        keep_unmapped = FALSE
      )
    ) |>
    filter(!is.na(.data$phase))

  # Convert the grouping column after aggregation so ggplot treats identifiers
  # as categories while the data-processing steps can still compare raw values.
  participant_phase_trials[[grouping_variable]] <- as.factor(
    participant_phase_trials[[grouping_variable]]
  )

  if (nrow(participant_phase_trials) == 0L) {
    stop(
      "No mapped trial counts were available after excluding intervention ",
      "and other phases.",
      call. = FALSE
    )
  }

  return(participant_phase_trials)
}

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
  ############################################################
  # 1) Prefer explicit study-design data
  ############################################################

  if (!is.null(sd)) {
    return(sd)
  }

  ############################################################
  # 2) Support interactive workflows with a study_design object
  ############################################################

  if (exists("study_design", envir = caller_env, inherits = TRUE)) {
    study_design_candidate <- get(
      "study_design",
      envir = caller_env,
      inherits = TRUE
    )

    if (is.data.frame(study_design_candidate)) {
      return(study_design_candidate)
    }
  }

  ############################################################
  # 3) Fall back to the bundled CSV
  ############################################################

  study_design_path <- system.file(
    "data",
    "study_design.csv",
    package = "fearbase",
    mustWork = FALSE
  )

  if (
    identical(study_design_path, "") &&
      file.exists(file.path("data", "study_design.csv"))
  ) {
    study_design_path <- file.path("data", "study_design.csv")
  }

  if (identical(study_design_path, "")) {
    stop(
      "`sd` must be supplied, an object named `study_design` must exist ",
      "in the calling environment, or bundled study-design data must be ",
      "available.",
      call. = FALSE
    )
  }

  readr::read_csv(study_design_path, show_col_types = FALSE)
}

#' Plot Trial Counts per Phase and Participant
#'
#' @description
#' Creates a faceted bar plot showing the distribution of phase-level trial
#' counts in a FEARBASE long-format data set. The plot can display either the
#' number of participants per trial count or the number of grouping units
#' (studies or conditions, depending on `grouping_variable`) per trial count.
#'
#' @param dl A data frame in long format. Must contain `participant_id`,
#'   `phase`, `stimulus`, `trial`, and the selected `grouping_variable` after
#'   `.apply_mapping_to_long_data()` is applied. If `NULL`, the function first
#'   attempts to use an object named `data_long` from the calling environment
#'   and then falls back to the package-bundled `data/data_long.csv` file.
#' @param y_axis A single character string selecting the plotted count. Use
#'   `"n"`, `"participant"`, or `"participants"` for participant counts; use
#'   `"s"`, `"study"`, or `"studies"` for grouping-unit counts.
#' @param grouping_variable A single character string specifying the grouping
#'   column. Must be one of `"condition_id"`, `"study_id"`,
#'   `"paper_cond_id"`, or `"paper_study_id"`.
#' @param cb A codebook data frame with at least `attribute`, `abbreviation`,
#'   and `name`. Rows where `attribute == "phase"` are used to translate phase
#'   abbreviations to display labels. If `NULL`, the function first attempts to
#'   use an object named `codebook` from the calling environment and then falls
#'   back to the package-bundled `data/codebook.csv` file.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Resolves omitted long-format data and codebook inputs.
#'   \item Applies the package-internal long-data mapping helper.
#'   \item Removes incomplete trial observations and the excluded `int` and
#'     `other` phase codes.
#'   \item Counts the maximum trial index per participant, phase, and stimulus.
#'   \item Sums stimulus-specific maxima to a participant-level phase total.
#'   \item Translates phase abbreviations with the codebook and applies a
#'     stable phase order.
#'   \item Returns a faceted `ggplot2` bar chart.
#' }
#'
#' Participant counts are based on distinct participant, phase, stimulus, and
#' trial rows. Grouping-unit counts are based on distinct combinations of the
#' selected grouping variable, phase, and trial total, so each grouping unit is
#' counted once per observed phase-level trial count.
#'
#' @return A `ggplot2` object with one facet row per phase.
#'
#' @examples
#' \dontrun{
#' trialsPhaseParticipant(data_long, cb = codebook)
#' trialsPhaseParticipant(
#'   data_long,
#'   y_axis = "participants",
#'   grouping_variable = "paper_study_id",
#'   cb = codebook
#' )
#' }
#'
#' @importFrom rlang .data
#' @export
trialsPhaseParticipant <- function(
  dl = NULL,
  y_axis = "s",
  grouping_variable = "condition_id",
  cb = NULL
) {
  ############################################################
  # 1) Prepare validated participant-level trial counts
  ############################################################

  # Normalize the y-axis selector once so the plotting branch can work with a
  # compact internal mode name.
  y_axis <- .validate_trials_phase_y_axis(y_axis)

  # Delegate data resolution, mapping, validation, trial counting, and phase
  # labeling to the shared helper. This keeps the public function focused on
  # plot construction.
  participant_phase_trials <- .prepare_trials_phase_participant_data(
    dl = dl,
    cb = cb,
    grouping_variable = grouping_variable,
    caller_env = parent.frame()
  )

  ############################################################
  # 2) Build the requested plot
  ############################################################

  if (identical(y_axis, "participants")) {
    # Count participants at each trial count within each phase and grouping
    # unit. The grouping fill makes cross-study or cross-condition differences
    # visible while preserving the phase facets.
    plot_data <- participant_phase_trials |>
      group_by(
        .data[[grouping_variable]],
        .data$phase,
        .data$trials
      ) |>
      summarise(
        n = n(),
        .groups = "drop"
      )

    graph <- plot_data |>
      ggplot(
        aes(
          x = .data$trials,
          y = .data$n,
          fill = .data[[grouping_variable]],
          group = .data[[grouping_variable]]
        )
      ) +
      geom_col(color = "white", linewidth = 0.2) +
      facet_grid(rows = vars(.data$phase), axes = "all", axis.labels = "all_x") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(
        x = "Number of Trials",
        y = "Number of Participants",
        fill = grouping_variable
      )
  } else {
    # Count each grouping unit once per phase and observed trial total. This
    # branch reports the distribution across studies or conditions, depending
    # on the selected grouping column.
    plot_data <- participant_phase_trials |>
      distinct(
        .data[[grouping_variable]],
        .data$phase,
        .data$trials
      ) |>
      group_by(
        .data$phase,
        .data$trials
      ) |>
      summarise(
        n = n(),
        .groups = "drop"
      )

    graph <- plot_data |>
      ggplot(
        aes(
          x = .data$trials,
          y = .data$n
        )
      ) +
      geom_col() +
      facet_grid(rows = vars(.data$phase), axes = "all", axis.labels = "all_x") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(
        x = "Number of Trials",
        y = "Number of Studies"
      )
  }

  # Return the plot object without printing so callers can add themes or save it
  # with ggplot2::ggsave().
  return(graph)
}

#' Compute Descriptive Statistics for Phase-Level Trial Counts
#'
#' @description
#' Computes descriptive statistics for participant-level trial counts within
#' each experimental phase in a FEARBASE long-format data set.
#'
#' @param dl A data frame in long format, or `NULL`. See
#'   `trialsPhaseParticipant()` for the required long-format columns and the
#'   omitted-data resolution rules.
#' @param grouping_variable A single character string specifying the grouping
#'   column. Must be one of `"condition_id"`, `"study_id"`,
#'   `"paper_cond_id"`, or `"paper_study_id"`.
#' @param cb A codebook data frame, or `NULL`. See `trialsPhaseParticipant()`
#'   for the required codebook columns and omitted-codebook resolution rules.
#'
#' @return A list-like object returned by `psych::describeBy()` with
#'   descriptive statistics grouped by codebook-derived phase label.
#'
#' @noRd
trialsPhaseParticipantDescriptive <- function(
  dl = NULL,
  grouping_variable = "condition_id",
  cb = NULL
) {
  ############################################################
  # 1) Reuse the plotting preparation path
  ############################################################

  # Use the same data preparation as trialsPhaseParticipant() so descriptive
  # statistics and plots are computed from identical phase-level trial counts.
  participant_phase_trials <- .prepare_trials_phase_participant_data(
    dl = dl,
    cb = cb,
    grouping_variable = grouping_variable,
    caller_env = parent.frame()
  )

  ############################################################
  # 2) Compute phase-wise descriptive statistics
  ############################################################

  # psych::describeBy() returns the package's established descriptive-statistic
  # format while grouping on the display-ready phase factor.
  result <- psych::describeBy(
    participant_phase_trials$trials,
    group = participant_phase_trials$phase
  )

  return(result)
}

#' Plot Study-Design Trial Counts per Phase
#'
#' @description
#' Creates a faceted bar plot of phase-level trial counts from the study-design
#' table, where phases are labeled dynamically from the FEARBASE codebook.
#'
#' @param sd A study-design data frame, or `NULL`. Must contain `study_id`, `name`,
#'   `cspTrials`, and `csmTrials` after `.apply_mapping_to_study_design()` is
#'   applied. The `name` column contains phase abbreviations. If `NULL`, the
#'   function first attempts to use an object named `study_design` from the
#'   calling environment and then falls back to the package-bundled
#'   `data/study_design.csv` file.
#' @param cb A codebook data frame, or `NULL`. If `NULL`, the function first
#'   attempts to use an object named `codebook` from the calling environment and
#'   then falls back to the package-bundled `data/codebook.csv` file.
#'
#' @return A `ggplot2` object with one facet row per phase.
#'
#' @noRd
studyDesign <- function(sd = NULL, cb = NULL) {
  ############################################################
  # 1) Resolve, map, and validate study-design inputs
  ############################################################

  # Capture the unevaluated expression so legacy calls such as
  # studyDesign(study_design) can fall back to bundled data when `study_design`
  # is not an object in the current test or interactive environment.
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
    facet_grid(rows = vars(.data$name), axes = "all", axis.labels = "all_x") +
    scale_x_continuous(breaks = scales::extended_breaks(10)) +
    labs(x = "Number of Trials", y = "Number of Studies")

  return(graph)
}
