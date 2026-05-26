#' Data collection year
#'
#' @param md metadata
#'
#' @return A ggplot object.
#' @export
dataCollectionYear <- function(md) {
  md <- .apply_mapping_to_metadata(md)

  data_collection_year <- md |>
    select(year) |>
    table() |>
    as_tibble() |>
    mutate(year = as.numeric(year))

  graph <- data_collection_year |>
    ggplot(aes(x = year, y = n)) +
    geom_col(fill = "#0032A0") +
    labs(x = "Year of Publication", y = "Number of Studies") +
    scale_x_continuous(breaks = ~ round(seq(min(.x), max(.x))))

  return(graph)
}


# TODO: number of studies vs. number of conditions?
# TODO: dynamic plot y axis title ("Number of Studies" vs. "Number of Datasets"?)
