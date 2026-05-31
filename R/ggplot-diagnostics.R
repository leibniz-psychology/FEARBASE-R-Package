#' Build a Count Axis Title from a Grouping Identifier
#'
#' @param grouping_variable Grouping identifier used for counts.
#'
#' @return A scalar character axis title.
#' @noRd
.count_axis_title <- function(grouping_variable) {
  if (identical(grouping_variable, "study_id")) {
    return("Number of Studies")
  }

  if (identical(grouping_variable, "condition_id")) {
    return("Number of Conditions")
  }

  grouping_variable |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()
}

#' Compute a Positive Upper Limit for Count Labels
#'
#' @param x Numeric count vector.
#' @param multiplier Expansion multiplier applied to the maximum count.
#' @param minimum Minimum returned limit.
#'
#' @return A finite positive numeric upper limit.
#' @noRd
.expanded_count_limit <- function(x, multiplier = 1.2, minimum = 1) {
  upper_limit <- max(x, na.rm = TRUE) * multiplier

  if (!is.finite(upper_limit) || upper_limit <= 0) {
    return(minimum)
  }

  upper_limit
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
trace_removed_rows <- function(
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
    filter(.data$.removed)
}
