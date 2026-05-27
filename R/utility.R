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
reorderPhases <- function(phases, order) {
  unique_phases <- unique(as.character(phases))
  priority_phases <- order
  existing_priority <- priority_phases[priority_phases %in% unique_phases]
  other_phases <- setdiff(unique_phases, priority_phases)
  phase_levels <- c(existing_priority, other_phases)

  return(factor(phases, levels = phase_levels))
}

#' Resolve the FEARBASE Codebook
#'
#' This internal helper centralizes the package convention for optional
#' codebook arguments. Explicit user input is preferred, then an interactive
#' caller-side object named `codebook`, and finally the package-bundled
#' `data/codebook.csv` file.
#'
#' @param cb A codebook data frame supplied by the caller, or `NULL`.
#' @param caller_env The environment to inspect for an object named `codebook`.
#'
#' @return A codebook data frame.
#' @noRd
.resolve_codebook <- function(cb = NULL, caller_env = parent.frame()) {
  ############################################################
  # 1) Prefer explicitly supplied data
  ############################################################

  # Explicit arguments make analyses reproducible and should always override
  # session objects or bundled convenience files.
  if (!is.null(cb)) {
    return(cb)
  }

  ############################################################
  # 2) Support interactive workflows with a caller-side codebook object
  ############################################################

  # Several package functions are used interactively with the FEARBASE data
  # objects already loaded into the workspace. Reuse such an object when it has
  # the rectangular structure needed by the codebook-based label helpers.
  if (exists("codebook", envir = caller_env, inherits = TRUE)) {
    codebook_candidate <- get(
      "codebook",
      envir = caller_env,
      inherits = TRUE
    )

    if (is.data.frame(codebook_candidate)) {
      return(codebook_candidate)
    }
  }

  ############################################################
  # 3) Fall back to the bundled CSV in installed and local package contexts
  ############################################################

  # Use system.file() for installed packages, then a repository-relative path
  # for local development and test runs before the package has been installed.
  codebook_path <- system.file(
    "data",
    "codebook.csv",
    package = "fearbase",
    mustWork = FALSE
  )

  if (
    identical(codebook_path, "") &&
      file.exists(file.path("data", "codebook.csv"))
  ) {
    codebook_path <- file.path("data", "codebook.csv")
  }

  if (identical(codebook_path, "")) {
    stop(
      "`cb` must be supplied, an object named `codebook` must exist ",
      "in the calling environment, or bundled codebook data must be ",
      "available.",
      call. = FALSE
    )
  }

  readr::read_csv(codebook_path, show_col_types = FALSE)
}

#' Build a Display-Label Lookup from the FEARBASE Codebook
#'
#' @param cb A codebook data frame.
#' @param attribute A single codebook attribute to select.
#' @param value_col Name of the output column containing abbreviations.
#' @param label_col Name of the output column containing display labels.
#' @param title_case Logical. If `TRUE`, convert labels to title case.
#'
#' @return A tibble with the requested abbreviation and label columns.
#' @noRd
.get_codebook_label_mapping <- function(
  cb,
  attribute,
  value_col,
  label_col,
  title_case = TRUE
) {
  ############################################################
  # 1) Validate the codebook contract used by all label helpers
  ############################################################

  .validate_data_frame(cb, "cb")
  .validate_required_columns(cb, c("attribute", "abbreviation", "name"), "cb")
  .validate_single_column_name(attribute, "attribute")
  .validate_single_column_name(value_col, "value_col")
  .validate_single_column_name(label_col, "label_col")
  .validate_logical_scalar(title_case, "title_case")

  ############################################################
  # 2) Select and normalize the requested codebook rows
  ############################################################

  # Keep a compact two-column lookup so joins remain explicit at call sites.
  # distinct() protects downstream joins from duplicated codebook rows.
  label_mapping <- cb |>
    filter(.data$attribute == attribute) |>
    select(
      "{value_col}" := "abbreviation",
      "{label_col}" := "name"
    ) |>
    filter(
      !is.na(.data[[value_col]]),
      !is.na(.data[[label_col]])
    ) |>
    distinct()

  if (isTRUE(title_case)) {
    label_mapping[[label_col]] <- stringr::str_to_title(
      label_mapping[[label_col]]
    )
  }

  if (nrow(label_mapping) == 0L) {
    stop(
      "No `",
      attribute,
      "` rows with abbreviations and names were found in `cb`.",
      call. = FALSE
    )
  }

  return(label_mapping)
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

  reorderPhases(phase_labels, defined_order)
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


#' Trace removed rows for a ggplot layer
#'
#' @description
#' Rebuilds a ggplot object and returns the source rows for one layer that are
#' likely to be dropped because required aesthetics are missing or because one
#' or more position aesthetics fall outside the trained panel range.
#'
#' @param plot A ggplot object.
#' @param layer The layer index to inspect.
#' @param data The data frame that feeds the selected layer. Defaults to
#'   `plot$data`.
#' @param row_id_col Name of the helper row id column added internally.
#'
#' @return A tibble with the original data, built coordinates prefixed with
#'   `.built_`, and diagnostic columns `.removed` and `.reason`.
#' @export
traceRemovedRows <- function(
  plot,
  layer = 1,
  data = plot$data,
  row_id_col = ".row_id"
) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object.")
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  if (length(layer) != 1 || !is.numeric(layer) || layer < 1) {
    stop("`layer` must be a single positive layer index.")
  }

  layer <- as.integer(layer)

  if (layer > length(plot$layers)) {
    stop("`layer` is larger than the number of plot layers.")
  }

  source_data <- tibble::as_tibble(data)

  if (row_id_col %in% names(source_data)) {
    stop("`row_id_col` already exists in `data`.")
  }

  source_data <- tibble::rowid_to_column(source_data, var = row_id_col)

  plot_with_ids <- plot
  plot_with_ids$data <- source_data

  built_plot <- ggplot_build(plot_with_ids)
  built_layer <- tibble::as_tibble(built_plot$data[[layer]])

  if (nrow(built_layer) != nrow(source_data)) {
    stop(
      paste0(
        "The selected layer does not map 1:1 to the supplied `data` (",
        nrow(source_data),
        " source rows vs ",
        nrow(built_layer),
        " built rows). Supply the exact per-layer input data."
      )
    )
  }

  required_aes <- plot$layers[[layer]]$geom$required_aes
  required_aes <- unique(unlist(strsplit(required_aes, "\\|", fixed = FALSE)))
  required_aes <- intersect(required_aes, names(built_layer))

  position_cols <- intersect(
    c(
      "x",
      "xmin",
      "xmax",
      "xend",
      "xintercept",
      "y",
      "ymin",
      "ymax",
      "yend",
      "yintercept"
    ),
    names(built_layer)
  )

  x_aes <- built_plot$layout$panel_params[[1]]$x$aesthetics
  y_aes <- built_plot$layout$panel_params[[1]]$y$aesthetics

  axis_for_col <- function(col_name) {
    if (col_name %in% x_aes) {
      return("x")
    }
    if (col_name %in% y_aes) {
      return("y")
    }
    NA_character_
  }

  ranges_for_row <- function(panel_id, axis_name) {
    panel_params <- built_plot$layout$panel_params[[panel_id]]
    if (is.null(panel_params)) {
      return(c(NA_real_, NA_real_))
    }

    range_name <- paste0(axis_name, ".range")
    range_vals <- panel_params[[range_name]]

    if (is.null(range_vals)) {
      return(c(NA_real_, NA_real_))
    }

    range_vals
  }

  outside_reason <- function(row_index, col_name) {
    value <- built_layer[[col_name]][row_index]

    if (!is.numeric(value) || is.na(value)) {
      return(FALSE)
    }

    axis_name <- axis_for_col(col_name)
    if (is.na(axis_name)) {
      return(FALSE)
    }

    panel_id <- built_layer$PANEL[row_index]
    panel_id <- as.integer(as.character(panel_id))
    axis_range <- ranges_for_row(panel_id, axis_name)

    if (any(is.na(axis_range))) {
      return(FALSE)
    }

    value < axis_range[1] || value > axis_range[2]
  }

  reasons <- vector("list", nrow(built_layer))

  for (i in seq_len(nrow(built_layer))) {
    row_reasons <- character(0)

    for (col_name in required_aes) {
      if (is.na(built_layer[[col_name]][i])) {
        row_reasons <- c(row_reasons, paste0("missing_", col_name))
      }
    }

    for (col_name in position_cols) {
      if (outside_reason(i, col_name)) {
        row_reasons <- c(row_reasons, paste0("outside_", col_name, "_range"))
      }
    }

    reasons[[i]] <- unique(row_reasons)
  }

  built_names <- paste0(".built_", names(built_layer))
  names(built_layer) <- built_names

  result <- bind_cols(source_data, built_layer) |>
    mutate(
      .removed = lengths(reasons) > 0,
      .reason = vapply(reasons, paste, collapse = "; ", character(1))
    )

  result |>
    filter(.removed)
}
