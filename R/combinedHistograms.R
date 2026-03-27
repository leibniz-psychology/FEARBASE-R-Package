#' @title build combined histogram of measures
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import tidyr
#' @import tibble
#' @import forcats
#' @import patchwork
#' @export
measuresHeatmap <- function() {
  dl <- getDataLong()
  md <- getMetadata()

  fulld <- dl |>
    left_join(md, by = "condition_id")

  # Define categories
  quest_measures <- c(
    "stais",
    "stait",
    "neo-ffi",
    "asi",
    "promis",
    "ius",
    "pswq"
  )
  phys_measures <- c("scr", "scl", "ps", "fps", "hr")
  rate_measures <- c("anx", "arous", "aware", "expect", "fear", "val", "stress")

  # Categorize and deduplicate
  measure_data <- fulld |>
    filter(
      measure %in% c(quest_measures, phys_measures, rate_measures)
    ) |>
    mutate(
      type = case_when(
        measure %in% quest_measures ~ "questionnaire",
        measure %in% phys_measures ~ "physiological",
        measure %in% rate_measures ~ "rating"
      )
    ) |>
    mutate(
      type = factor(
        type,
        levels = c("physiological", "rating", "questionnaire")
      )
    ) |>
    select(condition_id, participant_id, measure, type) |>
    distinct()

  # Prepare summary for bar plot
  measure_cat <- measure_data |>
    group_by(measure, type) |>
    summarise(used = n(), .groups = "drop") |>
    arrange(type, desc(used))

  # Set the order of measures based on the arranged summary
  measure_levels <- measure_cat$measure
  measure_cat$measure <- factor(
    measure_cat$measure,
    levels = rev(measure_levels)
  )

  # Compute co-occurrence
  co_data <- measure_data |>
    group_by(condition_id, measure) |>
    summarise(n = n(), .groups = "drop")

  crosstable_long <- .get_co_occurrence_data(
    co_data,
    "measure",
    "condition_id",
    "n"
  )

  # Apply the same levels to the heatmap data
  # x-axis from left to right, y-axis from top to bottom
  crosstable_long$measure <- factor(
    crosstable_long$measure,
    levels = measure_levels
  )
  crosstable_long$measure2 <- factor(
    crosstable_long$measure2,
    levels = rev(measure_levels)
  )

  # Heatmap
  hm <- .plot_co_occurrence_heatmap(
    crosstable_long,
    "measure",
    "measure2",
    "value",
    diag_na = TRUE
  )

  # Bar plot
  bp <- .plot_horizontal_bar(measure_cat, "measure", "used", fill_var = "type")

  # Combine
  return(c(hm, bp))
}

#' @title build histogram of phases
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import tidyr
#' @import tibble
#' @import patchwork
#' @export
phasesHeatmap <- function() {
  data_long <- getDataLong()

  # Prepare phase data
  phase_data <- data_long |>
    select(condition_id, participant_id, phase) |>
    distinct() |>
    drop_na(phase) |>
    filter(phase != "int") |>
    group_by(condition_id, phase) |>
    summarise(used = n(), .groups = "drop")

  # Summary for bar plot
  phase_summary <- phase_data |>
    group_by(phase) |>
    summarise(used = sum(used), .groups = "drop")

  phase_summary$phase <- forcats::fct_rev(reorderPhases(phase_summary$phase))

  # Compute co-occurrence
  crosstable_long <- .get_co_occurrence_data(
    phase_data,
    "phase",
    "condition_id",
    "used"
  )

  # Apply the same levels to the heatmap data
  crosstable_long$phase <- reorderPhases(crosstable_long$phase)
  crosstable_long$phase2 <- forcats::fct_rev(reorderPhases(
    crosstable_long$phase2
  ))

  # Heatmap
  hm <- .plot_co_occurrence_heatmap(
    crosstable_long,
    "phase",
    "phase2",
    "value",
    diag_na = TRUE
  )

  # Bar plot
  bp <- .plot_horizontal_bar(phase_summary, "phase", "used")

  # Combine
  return(c(hm, bp))
}

# --- Internal Helper Functions ---

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

.plot_co_occurrence_heatmap <- function(
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
    geom_label(
      aes(label = .data[[value_var]]),
      # size = rel(3),
      color = "black",
      # fontface = 'bold',
      fill = "white"
    ) +
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

.plot_horizontal_bar <- function(df, cat_var, count_var, fill_var = NULL) {
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
    geom_label(
      aes(label = .data[[count_var]], y = 5),
      hjust = 0,
      color = "black",
      # fontface = 'bold',
      fill = "white"
    ) +
    coord_flip() +
    theme_void() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
}

.arrange_histogram_layout <- function(hm, bp) {
  hm + bp + plot_layout(widths = c(1, 0.5))
}
