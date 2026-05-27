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
#'   \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} plots.
#'
#' @seealso \code{\link{phasesHeatmap}},
#'   \code{\link{plot_co_occurrence_heatmap}},
#'   \code{\link{plot_horizontal_bar}}
#'
#' @importFrom dplyr arrange desc distinct filter group_by
#' @importFrom dplyr left_join mutate select summarise
#' @importFrom tidyr drop_na
#' @importFrom rlang .data
#' @export
measuresHeatmap <- function(dl, md, cb) {
  # TODO: add information about if a measure is aming for the CS or the US for the measures where this is interesting
  dl <- .apply_mapping_to_long_data(dl)
  md <- .apply_mapping_to_metadata(md)

  # Data Processing
  fulld <- dl |>
    left_join(md, by = "condition_id")

  measure_name_mapping <- cb |>
    filter(attribute == "measure") |>
    select(measure_short = abbreviation, measure_long = name) |> # measure_short for the abbreviations and measure_long for the names
    mutate(measure_long = stringr::str_to_title(measure_long)) # TODO: clean the codebook in the database to have cleaner naming

  measure_data <- fulld |>
    left_join(measure_name_mapping, by = c("measure" = "measure_short")) |>
    select(condition_id, participant_id, measure_long) |>
    distinct() |>
    drop_na(measure_long)

  # Prepare summary for bar plot
  measure_cat <- measure_data |>
    group_by(measure_long) |>
    summarise(used = n(), .groups = "drop") |>
    arrange(desc(used))

  # Set the order of measures based on the arranged summary
  measure_levels <- measure_cat$measure_long
  measure_cat$measure_long <- factor(
    measure_cat$measure_long,
    levels = rev(measure_levels)
  )

  # Compute co-occurrence
  co_data <- measure_data |>
    group_by(condition_id, measure_long) |>
    summarise(n = n(), .groups = "drop")

  crosstable_long <- .get_co_occurrence_data(
    co_data,
    "measure_long",
    "condition_id",
    "n"
  )

  # Apply the same levels to the heatmap data
  # x-axis from left to right, y-axis from top to bottom
  crosstable_long$measure_long <- factor(
    crosstable_long$measure_long,
    levels = measure_levels
  )
  crosstable_long$measure_long2 <- factor(
    crosstable_long$measure_long2,
    levels = rev(measure_levels)
  )

  data_measures <- list(
    heatmap_data = crosstable_long,
    barplot_data = measure_cat
  )

  # Heatmap
  hm <- plot_co_occurrence_heatmap(
    data_measures$heatmap_data,
    "measure_long",
    "measure_long2",
    "value",
    diag_na = TRUE
  )

  # Bar plot
  bp <- plot_horizontal_bar(
    data_measures$barplot_data,
    "measure_long",
    "used"
  )
  arrange_histogram_layout(hm, bp)
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
#' displayed order prioritizes Habituation, Acquisition, and Extinction; additional
#' phases are appended after those priority phases.
#'
#' @return A \code{patchwork} object combining two
#'   \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} plots.
#'
#' @seealso \code{\link{measuresHeatmap}},
#'   \code{\link{plot_co_occurrence_heatmap}},
#'   \code{\link{plot_horizontal_bar}}
#'
#' @importFrom dplyr distinct filter group_by left_join mutate select summarise
#' @importFrom tidyr drop_na
#' @importFrom rlang .data
#' @export
phasesHeatmap <- function(dl, cb) {
  dl <- .apply_mapping_to_long_data(dl)

  # Data Processing
  # Prepare Phase naming using the codebook
  phase_name_mapping <- cb |>
    filter(attribute == "phase") |>
    select(phase_short = abbreviation, phase_long = name) |> # phase_short for the abbreviations and phase_long for the names
    mutate(phase_long = stringr::str_to_title(phase_long)) # TODO: clean the codebook in the database to have cleaner naming

  # Prepare phase data
  phase_data <- dl |>
    select(condition_id, participant_id, phase) |>
    distinct() |>
    drop_na(phase) |>
    left_join(phase_name_mapping, by = c("phase" = "phase_short")) |>
    filter(phase != "int", phase != "other") |> # TODO: decide if "int" and "other" should be included
    group_by(condition_id, phase_long) |>
    summarise(used = n(), .groups = "drop")

  # Summary for bar plot
  data_phases_barplot <- phase_data |>
    group_by(phase_long) |>
    summarise(used = sum(used), .groups = "drop")

  # Define the order of phases. This is still hardcoded. TODO: Decide if this can be defined somewhere else. Maybe the order after hab, acq and ext is
  # no longer as impotant. "reorderPhase" takes the defined order and then adds any other phases that are not in the defined order at the end.
  # This way we can ensure that the important phases are always in the same order and the other phases are still included without having to update
  # the code every time a new phase is added.
  defined_order <- c(
    "Habituation",
    "Acquisition",
    "Extinction"
  )
  # Apply the fixed order of phases to the bar plot data
  data_phases_barplot$phase_long <- forcats::fct_rev(reorderPhases(
    data_phases_barplot$phase_long,
    defined_order
  ))

  # Compute co-occurrence
  data_phases_heatmap <- .get_co_occurrence_data(
    phase_data,
    "phase_long",
    "condition_id",
    "used"
  )

  # Apply the same order to the heatmap data
  data_phases_heatmap$phase_long <- reorderPhases(
    data_phases_heatmap$phase_long,
    defined_order
  )
  data_phases_heatmap$phase_long2 <- forcats::fct_rev(reorderPhases(
    data_phases_heatmap$phase_long2,
    defined_order
  ))

  data_phases <- list(
    heatmap_data = data_phases_heatmap,
    barplot_data = data_phases_barplot
  )

  # Heatmap
  hm <- plot_co_occurrence_heatmap(
    data_phases$heatmap_data,
    "phase_long",
    "phase_long2",
    "value",
    diag_na = TRUE
  )

  # Bar plot
  bp <- plot_horizontal_bar(data_phases$barplot_data, "phase_long", "used")
  arrange_histogram_layout(hm, bp)
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
#' @return A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object.
#'
#' @details
#' Zero-valued cells are overlaid in white and diagonal \code{NA} cells are
#' overlaid in grey when \code{diag_na = TRUE}. The text labels show the raw
#' values from \code{value_var}.
#'
#' @importFrom ggplot2 aes element_blank element_rect element_text geom_text
#' @importFrom ggplot2 geom_tile ggplot labs margin scale_alpha
#' @importFrom ggplot2 scale_color_manual scale_y_discrete theme
#' @importFrom grid unit
#' @importFrom rlang .data
#' @export
plot_co_occurrence_heatmap <- function(
  df,
  x_var,
  y_var,
  value_var,
  diag_na = FALSE
) {
  if (diag_na) {
    df <- df |>
      mutate(
        !!value_var := ifelse(
          .data[[x_var]] == .data[[y_var]],
          NA,
          .data[[value_var]]
        )
      )
  }

  df <- df |>
    mutate(
      fill_white = ifelse(.data[[value_var]] > 0, 0, 1),
      fill_gray = ifelse(is.na(.data[[value_var]]), 1, 0),
      text_white = factor(
        ifelse(
          .data[[value_var]] > max(.data[[value_var]], na.rm = TRUE) / 4,
          "white",
          "black"
        ),
        levels = c("white", "black")
      )
    )

  ggplot(
    df,
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      fill = .data[[value_var]]
    )
  ) +
    geom_tile() +
    scale_y_discrete(position = "right") +
    geom_text(
      aes(label = .data[[value_var]]),
      # size = rel(3),
      color = "white",
      fontface = 'bold'
      # fill = "white"
    ) +
    geom_tile(
      aes(alpha = fill_white),
      fill = "white"
    ) +
    geom_tile(
      aes(alpha = fill_gray),
      fill = "gray50"
    ) +
    scale_color_manual(values = c("white", "black"), guide = "none") +
    scale_alpha(guide = "none") +
    labs(fill = "Number of Participants") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.key.width = unit(2, "line"),
      legend.title = element_text(margin = margin(r = 20)),
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
#' @return A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object.
#'
#' @importFrom ggplot2 aes coord_flip geom_col geom_text ggplot
#' @importFrom ggplot2 scale_fill_discrete theme theme_void
#' @importFrom rlang .data
#' @export
plot_horizontal_bar <- function(df, cat_var, count_var, fill_var = NULL) {
  p <- ggplot(
    df,
    aes(x = .data[[cat_var]], y = .data[[count_var]])
  )

  if (!is.null(fill_var)) {
    p <- p +
      aes(fill = .data[[fill_var]]) +
      geom_col() +
      scale_fill_discrete(palette = scales::pal_grey())
  } else {
    p <- p + geom_col(fill = "gray50")
  }

  p +
    geom_text(
      aes(label = .data[[count_var]]),
      hjust = -.1,
      color = "black",
    ) +
    coord_flip(ylim = c(0, max(df[[count_var]]) * 1.2)) +
    theme_void() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
}

#' Arrange a co-occurrence heatmap and marginal bar plot
#'
#' @param hm A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} heatmap object.
#' @param bp A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} bar plot object.
#'
#' @return A \code{patchwork} object with the heatmap on the left and the bar
#'   plot on the right.
#'
#' @importFrom patchwork plot_layout
#' @keywords internal
arrange_histogram_layout <- function(hm, bp) {
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
#' @importFrom dplyr all_of select
#' @importFrom tidyr pivot_longer pivot_wider
#' @keywords internal
.get_co_occurrence_data <- function(df, cat_var, id_var, count_var) {
  x_wide <- df |>
    select(all_of(c(cat_var, id_var, count_var))) |>
    pivot_wider(
      names_from = all_of(id_var),
      values_from = all_of(count_var),
      values_fill = 0
    )

  categories <- x_wide[[cat_var]]
  x_mat <- x_wide |> select(-all_of(cat_var)) |> as.matrix()

  crosstable_mat <- x_mat %*% t(x_mat > 0)

  crosstable <- as.data.frame(crosstable_mat)
  colnames(crosstable) <- categories
  crosstable[[cat_var]] <- categories

  crosstable_long <- crosstable |>
    pivot_longer(
      cols = -all_of(cat_var),
      names_to = paste0(cat_var, "2"),
      values_to = "value"
    )

  return(crosstable_long)
}
