#' Visualize Interactive Age Distributions from Long-Format Data
#'
#' Generates an interactive plotly visualization of participant age
#' distributions from a dataset in long format. The function supports exact-age
#' stacked histograms and ridge-style kernel density plots, optionally grouped by
#' a supported study or condition identifier.
#'
#' The input data must contain a column `measure` and a column `value`.
#' Only rows where `measure == "age"` are retained. The `value` column is
#' coerced to numeric to obtain age values. Rows with missing or non-coercible
#' age values are removed prior to plotting.
#'
#' @param dl A data frame in long format. Must contain at least the following
#'   columns:
#'   \itemize{
#'     \item `measure` (character): Variable identifier.
#'     \item `value` (numeric or character): Contains age values when
#'       `measure == "age"`.
#'     \item `participant_id` (numeric or character): Unique identifier of each
#'       participant.
#'     \item At least one of: `"condition_id"`, `"study_id"`,
#'       `"paper_cond_id"`, or `"paper_study_id"` when `grouping_variable` is
#'       supplied.
#'   }
#'   The function `.apply_mapping_to_long_data()` is applied internally before
#'   further processing.
#' @param type Character string specifying the plot type (case-insensitive).
#'   Must be one of `"histogram"`, `"hist"`, `"h"`, `"ridge"`, `"density"`, or
#'   `"r"`.
#' @param grouping_variable Optional character string specifying the grouping
#'   variable. If `NULL` (default), all valid age observations are plotted
#'   without grouping. If supplied, must be exactly one of `"condition_id"`,
#'   `"study_id"`, `"paper_cond_id"`, or `"paper_study_id"`.
#' @param save_html_widget A single logical value. If `TRUE`, the generated
#'   [plotly::plot_ly()] htmlwidget is additionally written to
#'   `age_interactive.html` in the current working directory before the widget
#'   object is returned. In OpenCPU sessions, files written to the working
#'   directory are exposed through the session `/files/` endpoint. The widget is
#'   saved as a self-contained HTML file.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Applies `.apply_mapping_to_long_data()` to `dl`.
#'   \item Filters rows where `measure == "age"`.
#'   \item Coerces `value` to numeric using `as.numeric()`.
#'   \item Removes rows with missing age values.
#'   \item If a grouping variable is supplied, converts it to a factor.
#'   \item If a grouping variable is supplied, orders factor levels by
#'     ascending mean age so the interactive display remains stable.
#'   \item Builds either exact-age bar traces or kernel-density traces with
#'     plotly.
#' }
#'
#' Histogram plots use exact observed age values rather than continuous bins,
#' matching [age()]. Grouped histograms are stacked by `grouping_variable` and
#' include the same group-level age summaries in their hover text as grouped
#' ridge plots. Ridge and density plots use [stats::density()] on the cleaned
#' numeric age values. Grouped ridge plots vertically offset one filled density
#' trace per group, label the y-axis with the ordered group levels, and include
#' participant count, mean, standard deviation, and range in the hover text for
#' each grouping level.
#'
#' @return A plotly htmlwidget.
#'
#' @section Dependencies:
#' This function requires:
#' \itemize{
#'   \item `dplyr`
#'   \item `plotly`
#'   \item `htmlwidgets` when `save_html_widget = TRUE`
#' }
#'
#' @examples
#' \dontrun{
#' # Ungrouped interactive histogram
#' age_interactive(dl)
#'
#' # Interactive histogram grouped by study
#' age_interactive(dl, type = "histogram", grouping_variable = "study_id")
#'
#' # Interactive ridge-density plot grouped by condition
#' age_interactive(dl, type = "ridge", grouping_variable = "condition_id")
#'
#' # Save a self-contained HTML widget in the current working directory
#' age_interactive(dl, save_html_widget = TRUE)
#' }
#'
#' @importFrom rlang .data
#' @export
age_interactive <- function(
  dl,
  type = "histogram",
  grouping_variable = NULL,
  save_html_widget = FALSE
) {
  ############################################################
  # 1) Validate user-facing controls
  ############################################################

  # Keep the save flag strict because it controls a filesystem side effect.
  .validate_logical_scalar(save_html_widget, "save_html_widget")

  # The interactive implementation uses native plotly traces. Checking for the
  # namespace here produces a clear package-level message if plotly is not
  # available in an installed environment.
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop(
      "Package `plotly` is required to use `age_interactive()`.",
      call. = FALSE
    )
  }

  ############################################################
  # 2) Prepare and validate age data using the static age() contract
  ############################################################

  # Validate before applying package-level mapping so non-data-frame inputs fail
  # with a direct package message instead of a lower-level mapping error.
  .validate_data_frame(dl, "dl")

  # Apply the same mapping helper used by age() so the static and interactive
  # functions agree on identifier columns and legacy input support.
  dl <- .apply_mapping_to_long_data(dl)

  valid_group_vars <- c(
    "condition_id",
    "study_id",
    "paper_cond_id",
    "paper_study_id"
  )

  has_grouping_variable <- !is.null(grouping_variable)

  if (has_grouping_variable) {
    grouping_variable <- .validate_choice(
      grouping_variable,
      "grouping_variable",
      valid_group_vars
    )

    if (!grouping_variable %in% names(dl)) {
      stop(
        "`grouping_variable` not found in `dl`.",
        call. = FALSE
      )
    }
  }

  .validate_single_column_name(type, "type")
  type <- tolower(type)

  valid_hist <- c("histogram", "hist", "h")
  valid_ridge <- c("ridge", "density", "r")

  if (!type %in% c(valid_hist, valid_ridge)) {
    stop(
      "`type` must be one of: histogram, hist, h, ridge, density, r.",
      call. = FALSE
    )
  }

  required_cols <- c("measure", "value", "participant_id")
  .validate_required_columns(dl, required_cols, "dl")

  # Build the same compact age-only table as age(). Keeping this local and
  # explicit makes the interactive tool independent from static ggplot output
  # while preserving the same data-cleaning behavior.
  data_age <- dl |>
    filter(.data$measure == "age") |>
    select(
      any_of(valid_group_vars),
      "participant_id",
      "value",
      "measure"
    ) |>
    mutate(
      age = suppressWarnings(as.numeric(.data$value)),
      across(
        any_of(valid_group_vars),
        as.factor
      )
    ) |>
    filter(!is.na(.data$age))

  if (nrow(data_age) == 0L) {
    stop("No valid age data found after filtering.", call. = FALSE)
  }

  if (has_grouping_variable) {
    # Order groups by mean age so histogram legends and ridge rows are
    # comparable across repeated calls with the same input data.
    group_order <- data_age |>
      group_by(.data[[grouping_variable]]) |>
      summarise(
        mean_age = mean(.data$age, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(.data$mean_age) |>
      pull(all_of(grouping_variable))

    data_age[[grouping_variable]] <- factor(
      data_age[[grouping_variable]],
      levels = group_order
    )
  }

  legend_label <- NULL

  if (has_grouping_variable) {
    legend_label <- grouping_variable |>
      stringr::str_to_title() |>
      stringr::str_replace(pattern = "_id", replacement = " ID")
  }

  ############################################################
  # 3) Build the requested native plotly widget
  ############################################################

  if (type %in% valid_hist) {
    graph_widget <- .build_age_histogram_interactive(
      data_age = data_age,
      grouping_variable = grouping_variable,
      has_grouping_variable = has_grouping_variable,
      legend_label = legend_label
    )
  } else {
    graph_widget <- .build_age_density_interactive(
      data_age = data_age,
      grouping_variable = grouping_variable,
      has_grouping_variable = has_grouping_variable,
      legend_label = legend_label
    )
  }

  ############################################################
  # 4) Optionally save the htmlwidget for OpenCPU/browser retrieval
  ############################################################

  # This mirrors the peak-detection interactive helper's file-in-working-
  # directory convention, but deliberately writes a self-contained HTML file as
  # requested so the result can be moved or opened without a sibling libdir.
  if (save_html_widget) {
    htmlwidgets::saveWidget(
      graph_widget,
      file = "plot.html",
      selfcontained = TRUE,
      libdir = "age_interactive_files"
    )
  }

  return(graph_widget)
}

#' Build the Interactive Age Histogram
#'
#' @param data_age Cleaned age data prepared by [age_interactive()].
#' @param grouping_variable Optional grouping-variable column name.
#' @param has_grouping_variable Logical flag indicating grouped output.
#' @param legend_label Optional legend label for grouped output.
#'
#' @return A plotly htmlwidget.
#' @noRd
.build_age_histogram_interactive <- function(
  data_age,
  grouping_variable,
  has_grouping_variable,
  legend_label
) {
  if (has_grouping_variable) {
    # Reuse the grouped summary payload from the ridge plot in histogram hover
    # text so both interactive age views expose the same descriptive context.
    group_summaries <- data_age |>
      .summarise_age_hover_groups(grouping_variable = grouping_variable)

    # Aggregate exact ages within each group before plotting so the trace hover
    # text reports the same participant counts as the static stacked bars.
    plot_data <- data_age |>
      group_by(
        .data$age,
        .data[[grouping_variable]]
      ) |>
      summarise(
        n = n(),
        .groups = "drop"
      ) |>
      arrange(
        .data[[grouping_variable]],
        .data$age
      ) |>
      mutate(
        group_level = as.character(.data[[grouping_variable]])
      ) |>
      left_join(
        group_summaries,
        by = "group_level"
      ) |>
      mutate(
        tooltip = paste0(
          legend_label,
          ": ",
          .data$group_level,
          "<br>Age: ",
          .data$age,
          "<br>Participants at this age: ",
          .data$n,
          "<br>Number of participants: ",
          .data$n_participants,
          "<br>Age mean: ",
          .format_age_hover_stat(.data$mean_age),
          "<br>Age standard deviation: ",
          .format_age_hover_stat(.data$sd_age),
          "<br>Age range: ",
          .format_age_hover_stat(.data$min_age),
          " - ",
          .format_age_hover_stat(.data$max_age)
        )
      )

    graph_widget <- plotly::plot_ly(
      data = plot_data,
      x = ~age,
      y = ~n,
      color = as.formula(paste0("~", grouping_variable)),
      colors = generate_palette(nlevels(data_age[[grouping_variable]])),
      type = "bar",
      text = ~tooltip,
      textposition = "none",
      hoverinfo = "text"
    ) |>
      plotly::layout(
        barmode = "stack",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Number of Participants"),
        legend = list(title = list(text = legend_label))
      )
  } else {
    # Build one overall summary row so ungrouped histogram hovers carry the
    # same descriptive-statistics block as grouped histogram hovers.
    age_summary <- data_age |>
      .summarise_age_hover_groups(grouping_variable = NULL)

    # Ungrouped output is a single trace, preserving a compact hover label and
    # avoiding a synthetic legend.
    plot_data <- data_age |>
      group_by(.data$age) |>
      summarise(
        n = n(),
        .groups = "drop"
      ) |>
      arrange(.data$age) |>
      mutate(
        tooltip = paste0(
          "Age: ",
          .data$age,
          "<br>Participants at this age: ",
          .data$n,
          "<br>Number of participants: ",
          age_summary$n_participants,
          "<br>Age mean: ",
          .format_age_hover_stat(age_summary$mean_age),
          "<br>Age standard deviation: ",
          .format_age_hover_stat(age_summary$sd_age),
          "<br>Age range: ",
          .format_age_hover_stat(age_summary$min_age),
          " - ",
          .format_age_hover_stat(age_summary$max_age)
        )
      )

    graph_widget <- plotly::plot_ly(
      data = plot_data,
      x = ~age,
      y = ~n,
      type = "bar",
      marker = list(color = generate_palette(1)),
      text = ~tooltip,
      textposition = "none",
      hoverinfo = "text"
    ) |>
      plotly::layout(
        xaxis = list(title = "Age"),
        yaxis = list(title = "Number of Participants"),
        showlegend = FALSE
      )
  }

  graph_widget
}

#' Build the Interactive Age Density or Ridge Plot
#'
#' @param data_age Cleaned age data prepared by [age_interactive()].
#' @param grouping_variable Optional grouping-variable column name.
#' @param has_grouping_variable Logical flag indicating grouped output.
#' @param legend_label Optional axis and legend label for grouped output.
#'
#' @return A plotly htmlwidget.
#' @noRd
.build_age_density_interactive <- function(
  data_age,
  grouping_variable,
  has_grouping_variable,
  legend_label
) {
  if (has_grouping_variable) {
    graph_widget <- plotly::plot_ly()
    group_levels <- levels(data_age[[grouping_variable]])
    group_colors <- generate_palette(length(group_levels))

    # Pre-compute one descriptive row per group so every hover box for a ridge
    # trace can show the group-level sample size and age distribution summary.
    group_summaries <- data_age |>
      .summarise_age_hover_groups(grouping_variable = grouping_variable)

    # Build one filled density polygon per group. Each density is vertically
    # offset by its group index, which creates a ridge-style plot while keeping
    # hover text native to plotly.
    for (group_index in seq_along(group_levels)) {
      current_group_level <- group_levels[[group_index]]
      group_values <- data_age |>
        filter(.data[[grouping_variable]] == current_group_level) |>
        pull(.data$age)

      density_data <- .compute_age_density_trace(
        age_values = group_values,
        y_offset = group_index
      )

      group_summary <- group_summaries |>
        filter(.data$group_level == .env$current_group_level)

      tooltip_prefix <- paste0(
        legend_label,
        ": ",
        current_group_level,
        "<br>Number of participants: ",
        group_summary$n_participants,
        "<br>Age mean: ",
        .format_age_hover_stat(group_summary$mean_age),
        "<br>Age standard deviation: ",
        .format_age_hover_stat(group_summary$sd_age),
        "<br>Age range: ",
        .format_age_hover_stat(group_summary$min_age),
        " - ",
        .format_age_hover_stat(group_summary$max_age)
      )

      # Store fully rendered hover text on each polygon coordinate. This is
      # more robust for plotly R filled traces than scalar hovertemplate strings,
      # which can fall back to showing only the trace name in some render paths.
      density_data <- density_data |>
        mutate(
          tooltip = paste0(
            tooltip_prefix,
            "<br>Age: ",
            .format_age_hover_stat(.data$age),
            "<br>Density: ",
            .format_age_hover_stat(.data$density)
          )
        )

      # Draw the filled ridge as a visual-only layer. Plotly can otherwise show
      # only the trace name when hovering filled polygons, even when text is
      # supplied. A second line layer below carries the detailed hover payload.
      graph_widget <- graph_widget |>
        plotly::add_trace(
          data = density_data,
          x = ~age,
          y = ~ridge_y,
          type = "scatter",
          mode = "lines",
          fill = "toself",
          line = list(color = group_colors[[group_index]], width = 1),
          fillcolor = group_colors[[group_index]],
          name = current_group_level,
          hoverinfo = "skip",
          showlegend = FALSE
        ) |>
        plotly::add_trace(
          data = density_data,
          x = ~age,
          y = ~ridge_y,
          type = "scatter",
          mode = "lines",
          fill = "toself",
          line = list(color = "rgba(0,0,0,0)", width = 0),
          fillcolor = "rgba(0,0,0,0)",
          name = current_group_level,
          text = tooltip_prefix,
          hovertext = tooltip_prefix,
          hoverinfo = "text",
          hoveron = "fills",
          hovertemplate = "%{hovertext}<extra></extra>",
          showlegend = FALSE
        ) |>
        plotly::add_trace(
          data = filter(density_data, .data$density > 0),
          x = ~age,
          y = ~ridge_y,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = group_colors[[group_index]], width = 2),
          marker = list(
            color = group_colors[[group_index]],
            opacity = 0,
            size = 8
          ),
          name = current_group_level,
          text = ~tooltip,
          hovertext = ~tooltip,
          hoverinfo = "text",
          hovertemplate = "%{hovertext}<extra></extra>",
          showlegend = FALSE
        )
    }

    graph_widget <- graph_widget |>
      plotly::layout(
        xaxis = list(title = "Age"),
        yaxis = list(
          title = legend_label,
          tickmode = "array",
          tickvals = seq_along(group_levels),
          ticktext = group_levels
        ),
        showlegend = FALSE
      )
  } else {
    density_data <- .compute_age_density_trace(
      age_values = data_age$age,
      y_offset = 0
    )

    graph_widget <- plotly::plot_ly(
      data = density_data,
      x = ~age,
      y = ~density,
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",
      line = list(color = generate_palette(1), width = 1),
      fillcolor = generate_palette(1),
      hovertemplate = paste0(
        "Age: %{x:.2f}",
        "<br>Density: %{y:.4f}",
        "<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(title = "Age"),
        yaxis = list(title = "Density"),
        showlegend = FALSE
      )
  }

  graph_widget
}

#' Summarise Age Values for Interactive Hover Text
#'
#' @param data_age Cleaned age data prepared by [age_interactive()].
#' @param grouping_variable Optional grouping-variable column name.
#'
#' @return A tibble with one descriptive-statistics row per group, or one row
#'   for the full data set when `grouping_variable = NULL`.
#' @noRd
.summarise_age_hover_groups <- function(data_age, grouping_variable = NULL) {
  if (!is.null(grouping_variable)) {
    data_age <- data_age |>
      group_by(.data[[grouping_variable]])
  }

  age_summaries <- data_age |>
    summarise(
      n_participants = n_distinct(.data$participant_id),
      mean_age = mean(.data$age, na.rm = TRUE),
      sd_age = stats::sd(.data$age, na.rm = TRUE),
      min_age = min(.data$age, na.rm = TRUE),
      max_age = max(.data$age, na.rm = TRUE),
      .groups = "drop"
    )

  if (!is.null(grouping_variable)) {
    # Store a character key for joins and loop lookups. This avoids factor-level
    # mismatches after the plotting order has been explicitly set upstream.
    age_summaries <- age_summaries |>
      mutate(
        group_level = as.character(.data[[grouping_variable]])
      ) |>
      select(
        -all_of(grouping_variable)
      )
  }

  age_summaries
}

#' Format Numeric Values for Age Hover Text
#'
#' @param x Numeric value to format.
#'
#' @return A single character string.
#' @noRd
.format_age_hover_stat <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }

  formatted_x <- formatC(x, format = "f", digits = 2)
  formatted_x[is.na(x) | !is.finite(x)] <- "NA"

  formatted_x
}

#' Compute a Density Trace for Interactive Age Plots
#'
#' @param age_values Numeric vector of cleaned age observations.
#' @param y_offset Numeric baseline for ridge-style density traces.
#'
#' @return A tibble with polygon coordinates and original density values.
#' @noRd
.compute_age_density_trace <- function(age_values, y_offset) {
  age_values <- age_values[is.finite(age_values)]

  # stats::density() requires at least two finite observations and non-zero
  # variance. When a group has a single unique age, draw a narrow triangular
  # spike so the group is still represented interactively rather than omitted.
  if (length(age_values) < 2L || length(unique(age_values)) < 2L) {
    center_age <- age_values[[1]]
    spike_width <- max(0.5, abs(center_age) * 0.01)

    density_tbl <- tibble::tibble(
      age = c(center_age - spike_width, center_age, center_age + spike_width),
      density = c(0, 1, 0)
    )
  } else {
    density_estimate <- stats::density(age_values, na.rm = TRUE)

    density_tbl <- tibble::tibble(
      age = density_estimate$x,
      density = density_estimate$y
    )
  }

  # Normalize grouped ridge heights so each trace has a comparable visual
  # amplitude even when sample sizes differ strongly between groups.
  max_density <- max(density_tbl$density, na.rm = TRUE)

  if (!is.finite(max_density) || max_density <= 0) {
    max_density <- 1
  }

  density_tbl |>
    mutate(
      ridge_y = y_offset + (.data$density / max_density) * 0.8
    ) |>
    bind_rows(
      tibble::tibble(
        age = rev(density_tbl$age),
        density = 0,
        ridge_y = y_offset
      )
    )
}
