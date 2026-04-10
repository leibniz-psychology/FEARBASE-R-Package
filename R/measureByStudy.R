#' @title distribution of measures across studies
#' @description
#' Compare the distribution of questionnaires or ratings across different studies.
#' @param measure_name The name(s) of the measure(s) (e.g., "stais", "age").
#'   If NULL (default), all measures in the data are shown.
#' @param split_by_stimulus Logical; if TRUE, distributions are split by stimulus.

measureByStudy <- function(
  dl = data_long,
  measure_name = NULL,
  split_by_stimulus = FALSE
) {
  # Filter by measure if provided
  if (!is.null(measure_name)) {
    dl <- dl |> filter(measure %in% measure_name)
  }

  # Prepare plot data: ensure value is numeric and exclude NAs
  plot_data <- dl |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value))

  if (nrow(plot_data) == 0) {
    if (is.null(measure_name)) {
      stop("No data found for any measures.")
    } else {
      stop(paste(
        "No data found for measure(s):",
        paste(measure_name, collapse = ", ")
      ))
    }
  }

  # Build the base plot
  p <- ggplot(
    plot_data,
    aes(x = as.factor(study_id), y = value)
  )

  # Add boxplot with appropriate fill mapping
  if (split_by_stimulus && "stimulus" %in% names(plot_data)) {
    p <- p +
      geom_boxplot(aes(fill = as.factor(stimulus))) +
      labs(fill = "Stimulus")
  } else {
    p <- p +
      geom_boxplot(aes(fill = as.factor(study_id))) +
      theme(legend.position = "none")
  }

  # Add common layers
  p <- p +
    coord_flip() +
    theme_minimal() +
    labs(
      title = if (is.null(measure_name)) {
        "Distribution of Measures by Study"
      } else {
        "Distribution of Measure(s) by Study"
      },
      x = "Study ID",
      y = "Value"
    )

  # Facet if multiple measures or if no specific measure was requested (default)
  # This ensures all measures are shown as facets when measure_name is NULL
  if (is.null(measure_name) || length(unique(plot_data$measure)) > 1) {
    p <- p + facet_wrap(~measure, scales = "free")
  }

  return(p)
}
