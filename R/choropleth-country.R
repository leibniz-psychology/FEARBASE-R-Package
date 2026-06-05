#' Prepare country-level counts for choropleth plots
#'
#' This internal helper normalizes country labels, prepares dataset-ID
#' tooltips, and computes the requested country-level count. Keeping the data
#' preparation separate from the map construction makes the public plotting
#' function easier to test without loading geospatial data.
#'
#' @param md Metadata data frame supplied to [choropleth_country()].
#' @param dl Long-format data frame supplied to [choropleth_country()], or
#'   `NULL` for non-participant counts.
#' @param country_variable A single country column name.
#' @param dataset_id_variable A single dataset identifier column name.
#' @param count A single count selector.
#'
#' @return A tibble with country names, counts, and tooltip text.
#' @noRd
.prepare_choropleth_country_counts <- function(
  md,
  dl = NULL,
  country_variable,
  dataset_id_variable,
  count = "datasets"
) {
  ############################################################
  # 1) Validate the metadata and required plotting columns
  ############################################################

  # The downstream country cleaning and aggregation use tidyverse column
  # selection, so fail early with package-style validation messages.
  .validate_data_frame(md, "md")
  .validate_single_column_name(country_variable, "country_variable")
  .validate_single_column_name(dataset_id_variable, "dataset_id_variable")
  count <- .validate_choice(
    count,
    "count",
    c("participants", "studies", "datasets")
  )

  required_cols <- c(country_variable, dataset_id_variable)
  .validate_required_columns(md, required_cols, "md")

  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop(
      "Package `countrycode` is required to clean country names. ",
      "Install it to use `choropleth_country()`.",
      call. = FALSE
    )
  }

  ############################################################
  # 2) Normalize metadata countries and prepare stable tooltips
  ############################################################

  metadata_country_lookup <- md |>
    mutate(
      country_clean = countrycode::countryname(
        .data[[country_variable]],
        nomatch = NA_character_
      ),
      dataset_id = as.character(.data[[dataset_id_variable]])
    ) |>
    select(all_of(c("country_clean", "dataset_id"))) |>
    filter(
      !is.na(.data$country_clean),
      !is.na(.data$dataset_id)
    )

  # Tooltips are always expressed in terms of the original metadata IDs. After
  # metadata mapping those IDs correspond to condition_id, but preserving the
  # original label keeps the interactive display consistent across count modes.
  country_tooltips <- metadata_country_lookup |>
    distinct(.data$country_clean, .data$dataset_id) |>
    arrange(.data$country_clean, .data$dataset_id) |>
    group_by(.data$country_clean) |>
    summarise(
      dataset_id_n = n_distinct(.data$dataset_id),
      dataset_ids = paste(.data$dataset_id, collapse = ", "),
      .groups = "drop"
    )

  if (nrow(country_tooltips) == 0L) {
    stop(
      "`md` must contain at least one non-missing country and dataset ",
      "identifier pair.",
      call. = FALSE
    )
  }

  ############################################################
  # 3) Compute the selected country-level count
  ############################################################

  if (identical(count, "datasets")) {
    # Count each metadata dataset once per country. This protects the map from
    # inflated counts when metadata contain repeated rows for one dataset.
    count_data <- metadata_country_lookup |>
      distinct(.data$country_clean, .data$dataset_id) |>
      group_by(.data$country_clean) |>
      summarise(
        n = n_distinct(.data$dataset_id),
        .groups = "drop"
      )
  } else if (identical(count, "studies")) {
    # Study counts require the package metadata mapping so condition-level
    # dataset IDs can be collapsed to their parent study_id values.
    metadata_mapped <- md |>
      .apply_mapping_to_metadata() |>
      mutate(
        country_clean = countrycode::countryname(
          .data[[country_variable]],
          nomatch = NA_character_
        )
      )

    .validate_required_columns(metadata_mapped, "study_id", "md")

    count_data <- metadata_mapped |>
      filter(
        !is.na(.data$country_clean),
        !is.na(.data$study_id)
      ) |>
      distinct(.data$country_clean, .data$study_id) |>
      group_by(.data$country_clean) |>
      summarise(
        n = n_distinct(.data$study_id),
        .groups = "drop"
      )
  } else {
    # Participant counts use long-format participant IDs, with country attached
    # from mapped metadata by condition_id.
    if (is.null(dl)) {
      stop(
        "`dl` must be supplied when `count = \"participants\"`.",
        call. = FALSE
      )
    }

    .validate_data_frame(dl, "dl")

    metadata_mapped <- md |>
      .apply_mapping_to_metadata() |>
      mutate(
        country_clean = countrycode::countryname(
          .data[[country_variable]],
          nomatch = NA_character_
        )
      )

    .validate_required_columns(
      metadata_mapped,
      c("condition_id", "country_clean"),
      "md"
    )

    dl_mapped <- .apply_mapping_to_long_data(dl)
    .validate_required_columns(
      dl_mapped,
      c("condition_id", "participant_id"),
      "dl"
    )

    country_by_condition <- metadata_mapped |>
      select(all_of(c("condition_id", "country_clean"))) |>
      filter(!is.na(.data$country_clean)) |>
      distinct()

    count_data <- dl_mapped |>
      select(all_of(c("condition_id", "participant_id"))) |>
      filter(!is.na(.data$participant_id)) |>
      left_join(country_by_condition, by = "condition_id") |>
      filter(!is.na(.data$country_clean)) |>
      distinct(.data$country_clean, .data$participant_id) |>
      group_by(.data$country_clean) |>
      summarise(
        n = n_distinct(.data$participant_id),
        .groups = "drop"
      )
  }

  country_counts <- count_data |>
    left_join(country_tooltips, by = "country_clean") |>
    filter(!is.na(.data$dataset_ids)) |>
    arrange(.data$n, .data$country_clean) |>
    mutate(
      country_clean = forcats::fct_reorder(.data$country_clean, .data$n),
      tooltip = if_else(
        .data$dataset_id_n > 1L,
        paste("Dataset IDs:", .data$dataset_ids),
        paste("Dataset ID:", .data$dataset_ids)
      )
    )

  if (nrow(country_counts) == 0L) {
    stop(
      "No non-missing country-level values were available for plotting.",
      call. = FALSE
    )
  }

  country_counts
}

#' Resolve the count-axis title for choropleth country plots
#'
#' @param count A validated count selector.
#'
#' @return A single character string.
#' @noRd
.choropleth_country_count_axis_title <- function(count) {
  switch(
    count,
    participants = "Number of Participants.",
    studies = "Number of Studies",
    datasets = "Number of Datasets"
  )
}

#' Compute readable count-axis breaks for choropleth country bars
#'
#' @param max_count Maximum observed country count.
#'
#' @return An integer vector of axis breaks.
#' @noRd
.choropleth_country_count_breaks <- function(max_count) {
  # Dense one-unit breaks work well for the small dataset and study counts that
  # the package typically displays, but participant counts can be much larger.
  # In those cases, sparse pretty breaks prevent tick labels from collapsing
  # into a dark band along the flipped count axis.
  if (is.na(max_count) || max_count <= 0L) {
    return(0L)
  }

  if (max_count <= 10L) {
    return(seq.int(0L, ceiling(max_count), by = 1L))
  }

  breaks <- pretty(c(0, max_count), n = 6)
  breaks <- breaks[breaks >= 0 & breaks <= max_count]

  unique(as.integer(round(breaks)))
}

#' Visualize FEARBASE country-level counts
#'
#' Creates an interactive country choropleth map with a linked horizontal bar
#' chart showing the number of distinct datasets, studies, or participants per
#' country.
#'
#' The metadata country labels are cleaned with [countrycode::countryname()],
#' counted according to `count`, joined to Natural Earth country geometries
#' from [rnaturalearth::ne_countries()], and rendered as a [ggiraph::girafe()]
#' htmlwidget. Hovering a country or bar highlights the matching element and
#' displays the metadata dataset identifiers in a tooltip.
#'
#' @param md A data frame containing study- or condition-level metadata.
#' @param dl A data frame in long format. Required when
#'   `count = "participants"` and ignored otherwise.
#' @param country_variable A single character string naming the column in `md`
#'   with country labels. Defaults to `"dataCountry"`.
#' @param dataset_id_variable A single character string naming the column in
#'   `md` with dataset or study identifiers. Defaults to `"id"`.
#' @param count A single character string specifying which country-level
#'   quantity to plot. Must be one of `"datasets"`, `"studies"`, or
#'   `"participants"`. Defaults to `"datasets"`.
#' @param save_html_widget A single logical value. If `TRUE`, the generated
#'   [ggiraph::girafe()] htmlwidget is additionally written to
#'   `choropleth_country.html` in the current working directory before the
#'   widget object is returned. In OpenCPU sessions, files written to the
#'   working directory are exposed through the session `/files/` endpoint.
#' @param include_bar A single logical value specifying whether the linked
#'   horizontal country-count bar chart is placed above the map. Defaults to
#'   `TRUE`.
#' @param map_scale A single character string passed to
#'   [rnaturalearth::ne_countries()] as `scale`. Defaults to `"large"`.
#' @param hover_css A single CSS string passed to
#'   [ggiraph::opts_hover()] for linked hover highlighting. Defaults to an
#'   orange fill with a black stroke.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Validates `md` and the selected metadata columns.
#'   \item Cleans country labels with [countrycode::countryname()].
#'   \item Removes metadata rows with missing cleaned countries or missing
#'     dataset IDs.
#'   \item Counts distinct dataset IDs, mapped `study_id` values, or
#'     long-format `participant_id` values per country.
#'   \item Joins those counts to Natural Earth country geometries.
#'   \item Builds an interactive map, optionally combines it with an
#'     interactive bar chart, and returns a [ggiraph::girafe()] widget.
#' }
#'
#' For `count = "studies"`, metadata mapping is applied before `study_id`
#' values are counted. For `count = "participants"`, metadata mapping supplies
#' condition-level country labels that are joined onto mapped long-format data
#' before unique `participant_id` values are counted. Tooltips always show the
#' original metadata dataset IDs, which correspond to `condition_id` after
#' metadata mapping. Countries that cannot be cleaned by `countrycode` are
#' excluded from the plotted counts.
#'
#' @return A [ggiraph::girafe()] htmlwidget showing country-level counts.
#'
#' @examples
#' \dontrun{
#' choropleth_country(metadata)
#' choropleth_country(
#'   metadata,
#'   country_variable = "country",
#'   dataset_id_variable = "study_id"
#' )
#' choropleth_country(metadata, data_long, count = "participants")
#' }
#'
#' @importFrom rlang .data
#' @export
choropleth_country <- function(
  md,
  dl = NULL,
  country_variable = "dataCountry",
  dataset_id_variable = "id",
  count = "datasets",
  save_html_widget = FALSE,
  include_bar = TRUE,
  map_scale = "large",
  hover_css = "fill:orange;stroke:black;"
) {
  ############################################################
  # 1) Validate user-facing controls and prepare country counts
  ############################################################

  # These scalar controls are passed directly to plotting and dependency calls,
  # so validate them before any external data are requested.
  .validate_logical_scalar(include_bar, "include_bar")
  .validate_logical_scalar(save_html_widget, "save_html_widget")
  .validate_single_column_name(map_scale, "map_scale")
  .validate_single_column_name(hover_css, "hover_css")
  count <- .validate_choice(
    count,
    "count",
    c("participants", "studies", "datasets")
  )

  required_packages <- c("ggiraph", "patchwork", "rnaturalearth", "sf")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0L) {
    stop(
      "Package(s) required for `choropleth_country()` are not installed: ",
      paste(missing_packages, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  country_counts <- .prepare_choropleth_country_counts(
    md = md,
    dl = dl,
    country_variable = country_variable,
    dataset_id_variable = dataset_id_variable,
    count = count
  )

  count_axis_title <- .choropleth_country_count_axis_title(count)
  count_axis_breaks <- .choropleth_country_count_breaks(max(country_counts$n))

  ############################################################
  # 2) Join country counts to Natural Earth map geometries
  ############################################################

  # Natural Earth uses country labels that are close to, but not always exactly
  # the same as, FEARBASE metadata labels. Cleaning both sides through
  # countrycode gives the join a stable shared key.
  world_sf <- rnaturalearth::ne_countries(
    scale = map_scale,
    returnclass = "sf"
  ) |>
    mutate(
      country_clean = countrycode::countryname(
        .data$sovereignt,
        nomatch = NA_character_
      )
    )

  world_sf_complete <- world_sf |>
    left_join(country_counts, by = "country_clean") |>
    filter(.data$country_clean != "Antarctica")

  ############################################################
  # 3) Create a stable linked colour palette for countries
  ############################################################

  # The notebook prototype sampled every fourth generated colour. Reusing that
  # spacing here keeps adjacent country colours visually separated while still
  # drawing from the package palette.
  n_countries <- nrow(country_counts)
  palette_spacing <- 4L
  palette_values <- generate_palette(n_countries * palette_spacing)
  country_colours <- stats::setNames(
    palette_values[seq(
      from = 1L,
      to = n_countries * palette_spacing,
      by = palette_spacing
    )],
    levels(country_counts$country_clean)
  )

  ############################################################
  # 4) Build the interactive map and optional bar chart
  ############################################################

  map_plot <- ggplot() +
    ggiraph::geom_sf_interactive(
      data = world_sf_complete,
      fill = "lightgrey",
      color = "lightgrey"
    ) +
    ggiraph::geom_sf_interactive(
      data = filter(world_sf_complete, !is.na(.data$n)),
      aes(
        fill = .data$country_clean,
        tooltip = .data$tooltip,
        data_id = .data$country_clean
      )
    ) +
    ggplot2::coord_sf(crs = sf::st_crs(3857)) +
    scale_fill_manual(values = country_colours) +
    theme_fearbase_void(background = "white") +
    theme(legend.position = "none")

  if (include_bar) {
    # Use the same fill scale and data_id as the map so ggiraph links hover
    # state between each country polygon and the matching count bar.
    bar_plot <- country_counts |>
      ggplot(
        aes(
          x = .data$country_clean,
          y = .data$n,
          fill = .data$country_clean,
          tooltip = .data$tooltip,
          data_id = .data$country_clean
        )
      ) +
      ggiraph::geom_col_interactive() +
      coord_flip() +
      scale_fill_manual(values = country_colours) +
      scale_y_continuous(
        name = count_axis_title,
        breaks = count_axis_breaks
      ) +
      labs(x = NULL) +
      guides(fill = "none") +
      theme_fearbase_dense(legend_position = "none")

    graph <- bar_plot / map_plot + patchwork::plot_layout(heights = c(1, 3))
  } else {
    graph <- map_plot
  }

  graph_widget <- ggiraph::girafe(ggobj = graph) |>
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = hover_css)
    )

  # OpenCPU captures ordinary static plots through its graphics device, but
  # htmlwidgets need to be written as HTML assets to become available from the
  # session /files/ endpoint. Keep the side effect optional so ordinary local
  # and Quarto calls still just receive the widget object.
  if (save_html_widget) {
    htmlwidgets::saveWidget(
      graph_widget,
      file = "choropleth_country.html",
      selfcontained = FALSE,
      libdir = "choropleth_country_files"
    )
  }

  return(graph_widget)
}
