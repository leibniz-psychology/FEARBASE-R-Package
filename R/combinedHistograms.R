#' Plot a co-occurrence heatmap for recorded outcome measures
#'
#' @description
#' Builds a combined heatmap and marginal bar plot showing how often outcome
#' measures occur together across mapped experimental conditions.
#'
#' @param dl A data frame containing long-format participant-level data. The
#'   data are passed through the package-internal mapping helper and must then
#'   contain \code{condition_id}, \code{participant_id}, and \code{measure}.
#' @param md A data frame containing study-level metadata. The data are passed
#'   through the package-internal mapping helper and must then contain
#'   \code{condition_id}.
#' @param cb A codebook data frame with at least \code{attribute},
#'   \code{abbreviation}, and \code{name}. Rows where
#'   \code{attribute == "measure"} are used to translate measure abbreviations
#'   to display labels.
#'
#' @details
#' The function first maps condition and study identifiers, joins the long data
#' to metadata by \code{condition_id}, translates measure abbreviations with the
#' codebook, and keeps one row per participant, condition, and measure. It then
#' computes pairwise measure co-occurrence counts across conditions and combines
#' the heatmap with a horizontal bar plot of marginal measure frequencies.
#'
#' @return A \code{patchwork} object combining two
#'   \code{\link[ggplot2:ggplot]{ggplot()}} plots.
#'
#' @seealso \code{\link{phasesHeatmap}},
#'   \code{\link{plot_co_occurrence_heatmap}},
#'   \code{\link{plot_horizontal_bar}}
#'
#' @importFrom rlang .data
#' @export
measuresHeatmap <- function(dl, md, cb) {
  # Validate the user-facing inputs before any internal mapping is applied.
  # This produces clear package-level errors instead of lower-level join or
  # tidy-evaluation errors.
  .validate_data_frame(dl, "dl")
  .validate_data_frame(md, "md")
  .validate_data_frame(cb, "cb")
  .validate_required_columns(cb, c("attribute", "abbreviation", "name"), "cb")

  # Normalize study and condition identifiers with the package mapping helpers.
  # The helper functions are intentionally called before column validation
  # because callers may supply raw database tables that still need mapping.
  dl <- .apply_mapping_to_long_data(dl)
  md <- .apply_mapping_to_metadata(md)

  .validate_required_columns(
    dl,
    c("condition_id", "participant_id", "measure"),
    "dl"
  )
  .validate_required_columns(md, "condition_id", "md")

  # Join participant-level observations to their mapped metadata rows. The
  # current measure co-occurrence calculation only needs the identifiers from
  # metadata, but the join preserves the established package data flow.
  full_data <- dl |>
    left_join(md, by = "condition_id")

  # Build a lookup table that turns compact measure codes into human-readable
  # labels for plotting. The labels are title-cased here because the current
  # codebook stores several names in database-friendly casing.
  measure_name_mapping <- cb |>
    filter(.data$attribute == "measure") |>
    select(
      measure_short = "abbreviation",
      measure_long = "name"
    ) |>
    mutate(
      measure_long = stringr::str_to_title(.data$measure_long)
    )

  # Keep one row per condition, participant, and translated measure. This
  # prevents repeated rows for the same participant-level measure from
  # overstating measure usage in the marginal bar plot or co-occurrence matrix.
  measure_data <- full_data |>
    left_join(
      measure_name_mapping,
      by = c("measure" = "measure_short")
    ) |>
    select(
      "condition_id",
      "participant_id",
      "measure_long"
    ) |>
    distinct() |>
    tidyr::drop_na("measure_long")

  if (nrow(measure_data) == 0) {
    stop(
      "No mapped measure values were available for plotting.",
      call. = FALSE
    )
  }

  # Count marginal measure usage for the right-hand bar plot. Sorting here also
  # defines the shared factor order used by both the bar plot and the heatmap.
  measure_cat <- measure_data |>
    group_by(.data$measure_long) |>
    summarise(used = n(), .groups = "drop") |>
    arrange(desc(.data$used))

  measure_levels <- measure_cat$measure_long
  measure_cat$measure_long <- factor(
    measure_cat$measure_long,
    levels = rev(measure_levels)
  )

  # Collapse participant rows to one count per condition and measure. The
  # helper below converts this long summary into pairwise co-occurrence counts.
  co_data <- measure_data |>
    group_by(
      .data$condition_id,
      .data$measure_long
    ) |>
    summarise(n = n(), .groups = "drop")

  crosstable_long <- .get_co_occurrence_data(
    co_data,
    "measure_long",
    "condition_id",
    "n"
  )

  # Use mirrored factor levels so the x-axis reads in descending marginal
  # frequency while the y-axis reads top-to-bottom in the same visual order.
  crosstable_long$measure_long <- factor(
    crosstable_long$measure_long,
    levels = measure_levels
  )
  crosstable_long$measure_long2 <- factor(
    crosstable_long$measure_long2,
    levels = rev(measure_levels)
  )

  # Compose the reusable heatmap and bar plot components into the package's
  # standard two-panel co-occurrence layout.
  heatmap_plot <- plot_co_occurrence_heatmap(
    crosstable_long,
    "measure_long",
    "measure_long2",
    "value",
    diag_na = TRUE
  )
  bar_plot <- plot_horizontal_bar(measure_cat, "measure_long", "used")

  arrange_histogram_layout(heatmap_plot, bar_plot)
}

#' Plot a co-occurrence heatmap for experimental phases
#'
#' @description
#' Builds a combined heatmap and marginal bar plot showing how often
#' experimental phases occur together across mapped experimental conditions.
#'
#' @param dl A data frame containing long-format participant-level data. The
#'   data are passed through the package-internal mapping helper and must then
#'   contain \code{condition_id}, \code{participant_id}, and \code{phase}.
#' @param cb A codebook data frame with at least \code{attribute},
#'   \code{abbreviation}, and \code{name}. Rows where
#'   \code{attribute == "phase"} are used to translate phase abbreviations to
#'   display labels.
#'
#' @details
#' The function maps condition and study identifiers, translates phase
#' abbreviations with the codebook, removes the currently excluded \code{int}
#' and \code{other} phase codes, and counts phase usage per condition. The
#' displayed order prioritizes Habituation, Acquisition, and Extinction;
#' additional phases are appended after those priority phases.
#'
#' @return A \code{patchwork} object combining two
#'   \code{\link[ggplot2:ggplot]{ggplot()}} plots.
#'
#' @seealso \code{\link{measuresHeatmap}},
#'   \code{\link{plot_co_occurrence_heatmap}},
#'   \code{\link{plot_horizontal_bar}}
#'
#' @export
phasesHeatmap <- function(dl, cb) {
  # Validate raw inputs before the mapping step so callers get concise errors
  # when a non-data-frame object is supplied.
  .validate_data_frame(dl, "dl")
  .validate_data_frame(cb, "cb")
  .validate_required_columns(cb, c("attribute", "abbreviation", "name"), "cb")

  # Normalize condition and study identifiers before validating mapped columns.
  dl <- .apply_mapping_to_long_data(dl)

  .validate_required_columns(
    dl,
    c("condition_id", "participant_id", "phase"),
    "dl"
  )

  # Build the condition-level phase summary. Each participant contributes once
  # per condition and phase before aggregation, and currently excluded phase
  # codes are removed before computing co-occurrences.
  phase_data <- dl |>
    select(
      "condition_id",
      "participant_id",
      "phase"
    ) |>
    distinct() |>
    tidyr::drop_na("phase") |>
    mutate(
      phase_long = .label_phases_from_codebook(
        .data$phase,
        cb = cb,
        keep_unmapped = FALSE
      )
    ) |>
    filter(
      !.data$phase %in% c("int", "other"),
      !is.na(.data$phase_long)
    ) |>
    group_by(
      .data$condition_id,
      .data$phase_long
    ) |>
    summarise(used = n(), .groups = "drop")

  if (nrow(phase_data) == 0) {
    stop(
      "No mapped phase values were available for plotting.",
      call. = FALSE
    )
  }

  # Count the marginal frequency of each phase across all mapped conditions for
  # the right-hand bar plot.
  data_phases_barplot <- phase_data |>
    group_by(.data$phase_long) |>
    summarise(
      used = sum(.data$used),
      .groups = "drop"
    )

  # Keep the core fear-conditioning sequence visually stable. Additional phase
  # labels remain available and are appended after this priority order by
  # reorderPhases().
  defined_order <- c(
    "Habituation",
    "Acquisition",
    "Extinction"
  )

  data_phases_barplot$phase_long <- forcats::fct_rev(
    reorderPhases(data_phases_barplot$phase_long, defined_order)
  )

  # Convert condition-level phase counts into a pairwise co-occurrence table for
  # the heatmap panel.
  data_phases_heatmap <- .get_co_occurrence_data(
    phase_data,
    "phase_long",
    "condition_id",
    "used"
  )

  # Apply the same priority phase order to both heatmap axes. The y-axis is
  # reversed so the top row aligns with the first visible bar plot category.
  data_phases_heatmap$phase_long <- reorderPhases(
    data_phases_heatmap$phase_long,
    defined_order
  )
  data_phases_heatmap$phase_long2 <- forcats::fct_rev(
    reorderPhases(data_phases_heatmap$phase_long2, defined_order)
  )

  heatmap_plot <- plot_co_occurrence_heatmap(
    data_phases_heatmap,
    "phase_long",
    "phase_long2",
    "value",
    diag_na = TRUE
  )
  bar_plot <- plot_horizontal_bar(data_phases_barplot, "phase_long", "used")

  arrange_histogram_layout(heatmap_plot, bar_plot)
}

#' Plot a square co-occurrence heatmap
#'
#' @description
#' Creates a tiled heatmap from long-format co-occurrence data.
#'
#' @param df A data frame containing one row per x/y category combination.
#' @param x_var A string naming the column to display on the x-axis.
#' @param y_var A string naming the column to display on the y-axis.
#' @param value_var A string naming the numeric column used for tile fill values
#'   and text labels.
#' @param diag_na Logical. If \code{TRUE}, cells where \code{x_var} and
#'   \code{y_var} have the same category are set to \code{NA} before plotting.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot()}} object.
#'
#' @details
#' Zero-valued cells are overlaid in white and diagonal \code{NA} cells are
#' overlaid in grey when \code{diag_na = TRUE}. The text labels show the raw
#' values from \code{value_var}.
#'
#' @export
plot_co_occurrence_heatmap <- function(
  df,
  x_var,
  y_var,
  value_var,
  diag_na = FALSE
) {
  # Validate plot inputs up front. These checks make the function safer to use
  # independently from measuresHeatmap() and phasesHeatmap().
  .validate_data_frame(df, "df")
  .validate_single_column_name(x_var, "x_var")
  .validate_single_column_name(y_var, "y_var")
  .validate_single_column_name(value_var, "value_var")
  .validate_logical_scalar(diag_na, "diag_na")
  .validate_required_columns(df, c(x_var, y_var, value_var), "df")

  if (!is.numeric(df[[value_var]])) {
    stop("`value_var` must identify a numeric column in `df`.", call. = FALSE)
  }

  # Optionally hide the diagonal because self-co-occurrence is not informative
  # in these summary plots.
  if (isTRUE(diag_na)) {
    df <- df |>
      mutate(
        "{value_var}" := if_else(
          .data[[x_var]] == .data[[y_var]],
          NA_real_,
          .data[[value_var]]
        )
      )
  }

  # Pre-compute helper columns used by overlay tiles and text styling. Keeping
  # them in the data makes the ggplot layers simple and easy to inspect.
  max_value <- max(df[[value_var]], na.rm = TRUE)
  if (!is.finite(max_value)) {
    max_value <- 0
  }

  plot_data <- df |>
    mutate(
      fill_white = if_else(.data[[value_var]] > 0, 0, 1),
      fill_gray = if_else(is.na(.data[[value_var]]), 1, 0),
      text_colour = factor(
        if_else(
          .data[[value_var]] > max_value / 4,
          "white",
          "black"
        ),
        levels = c("white", "black")
      )
    )

  # Draw the heatmap in three tile layers: the value layer, a white overlay for
  # zero counts, and a grey overlay for NA cells such as the diagonal.
  ggplot(
    plot_data,
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      fill = .data[[value_var]]
    )
  ) +
    geom_tile() +
    scale_y_discrete(position = "right") +
    geom_text(
      aes(
        label = .data[[value_var]],
        colour = .data$text_colour
      ),
      fontface = "bold"
    ) +
    geom_tile(
      aes(alpha = .data$fill_white),
      fill = "white"
    ) +
    geom_tile(
      aes(alpha = .data$fill_gray),
      fill = "gray50"
    ) +
    scale_colour_manual(
      values = c("white" = "white", "black" = "black"),
      guide = "none"
    ) +
    scale_alpha(guide = "none") +
    labs(fill = "Number of Participants") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.key.width = grid::unit(2, "line"),
      legend.title = element_text(
        margin = margin(r = 20)
      ),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank()
    )
}

#' Plot a horizontal count bar chart
#'
#' @description
#' Creates a horizontal bar chart for pre-aggregated category counts.
#'
#' @param df A data frame containing one row per category.
#' @param cat_var A string naming the categorical column to plot on the
#'   category axis.
#' @param count_var A string naming the numeric count column.
#' @param fill_var An optional string naming a column used for bar fill groups.
#'   If \code{NULL}, all bars are drawn in grey.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot()}} object.
#'
#' @export
plot_horizontal_bar <- function(df, cat_var, count_var, fill_var = NULL) {
  # Validate inputs so this exported plotting helper fails with clear messages
  # when it is used outside the higher-level heatmap functions.
  .validate_data_frame(df, "df")
  .validate_single_column_name(cat_var, "cat_var")
  .validate_single_column_name(count_var, "count_var")
  .validate_required_columns(df, c(cat_var, count_var), "df")

  if (!is.null(fill_var)) {
    .validate_single_column_name(fill_var, "fill_var")
    .validate_required_columns(df, fill_var, "df")
  }

  if (!is.numeric(df[[count_var]])) {
    stop("`count_var` must identify a numeric column in `df`.", call. = FALSE)
  }

  if (nrow(df) == 0) {
    stop("`df` must contain at least one row.", call. = FALSE)
  }

  # Give the text labels a small amount of headroom to the right of the bars.
  axis_upper_limit <- max(df[[count_var]], na.rm = TRUE) * 1.2
  if (!is.finite(axis_upper_limit) || axis_upper_limit <= 0) {
    axis_upper_limit <- 1
  }

  plot <- ggplot(
    df,
    aes(
      x = .data[[cat_var]],
      y = .data[[count_var]]
    )
  )

  # Optionally map a fill variable. Without a fill variable, all bars are drawn
  # in a neutral grey to keep the marginal plot visually subordinate.
  if (!is.null(fill_var)) {
    plot <- plot +
      aes(fill = .data[[fill_var]]) +
      geom_col() +
      scale_fill_discrete(palette = scales::pal_grey())
  } else {
    plot <- plot +
      geom_col(fill = "gray50")
  }

  plot +
    geom_text(
      aes(label = .data[[count_var]]),
      hjust = -0.1,
      color = "black"
    ) +
    coord_flip(ylim = c(0, axis_upper_limit)) +
    theme_void() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
}

#' Arrange a co-occurrence heatmap and marginal bar plot
#'
#' @param hm A \code{\link[ggplot2:ggplot]{ggplot()}} heatmap object.
#' @param bp A \code{\link[ggplot2:ggplot]{ggplot()}} bar plot object.
#'
#' @return A \code{patchwork} object with the heatmap on the left and the bar
#'   plot on the right.
#'
#' @keywords internal
arrange_histogram_layout <- function(hm, bp) {
  # Keep the heatmap visually dominant while reserving enough width for the
  # marginal counts on the right.
  hm + bp + plot_layout(widths = c(1, 0.5))
}

#' Compute long-format category co-occurrence counts
#'
#' @param df A data frame containing category, identifier, and count columns.
#' @param cat_var A string naming the category column.
#' @param id_var A string naming the identifier column across which categories
#'   co-occur.
#' @param count_var A string naming the numeric count column.
#'
#' @return A data frame with one row per category pair and a \code{value} column
#'   with the co-occurrence count.
#'
#' @keywords internal
.get_co_occurrence_data <- function(df, cat_var, id_var, count_var) {
  # Validate the summarized input before reshaping it. Matrix multiplication
  # requires a numeric count column and stable category and identifier columns.
  .validate_data_frame(df, "df")
  .validate_single_column_name(cat_var, "cat_var")
  .validate_single_column_name(id_var, "id_var")
  .validate_single_column_name(count_var, "count_var")
  .validate_required_columns(df, c(cat_var, id_var, count_var), "df")

  if (!is.numeric(df[[count_var]])) {
    stop("`count_var` must identify a numeric column in `df`.", call. = FALSE)
  }

  if (nrow(df) == 0) {
    stop("`df` must contain at least one row.", call. = FALSE)
  }

  # Reshape to a category-by-identifier matrix where each cell contains the
  # number of observations for that category in that identifier.
  x_wide <- df |>
    select(all_of(c(cat_var, id_var, count_var))) |>
    tidyr::pivot_wider(
      names_from = all_of(id_var),
      values_from = all_of(count_var),
      values_fill = 0
    )

  categories <- x_wide[[cat_var]]
  x_mat <- x_wide |>
    select(-all_of(cat_var)) |>
    as.matrix()

  # Matrix multiplication gives pairwise co-occurrence counts. The left matrix
  # preserves counts for the focal category, while the transposed logical matrix
  # indicates whether the paired category appears in each identifier.
  crosstable_mat <- x_mat %*% t(x_mat > 0)

  crosstable <- as.data.frame(crosstable_mat)
  colnames(crosstable) <- categories
  crosstable[[cat_var]] <- categories

  # Return the matrix in long format so ggplot can draw one tile per pair.
  crosstable |>
    tidyr::pivot_longer(
      cols = -all_of(cat_var),
      names_to = paste0(cat_var, "2"),
      values_to = "value"
    )
}

# Validate that an argument is a data frame before it enters a tidyverse
# pipeline. This keeps error messages tied to the public argument name.
.validate_data_frame <- function(x, arg_name) {
  if (!is.data.frame(x)) {
    stop("`", arg_name, "` must be a data frame.", call. = FALSE)
  }
}

# Validate required columns once at function boundaries. This avoids repeated
# ad hoc checks and makes all missing-column errors consistent.
.validate_required_columns <- function(data, required_cols, arg_name) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s) in `",
      arg_name,
      "`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

# Validate string column-name arguments used with the .data pronoun. Requiring a
# single non-empty string prevents ambiguous tidy-evaluation behavior.
.validate_single_column_name <- function(x, arg_name) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || identical(x, "")) {
    stop(
      "`",
      arg_name,
      "` must be a single non-empty character string.",
      call. = FALSE
    )
  }
}

# Validate scalar logical flags used to alter plotting behavior.
.validate_logical_scalar <- function(x, arg_name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("`", arg_name, "` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
}
