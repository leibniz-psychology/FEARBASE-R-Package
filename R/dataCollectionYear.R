#' Data collection year
#'
#' @param md metadata
#'
#' @return A ggplot object.
#' @export
dataCollectionYear <- function(md) {
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
