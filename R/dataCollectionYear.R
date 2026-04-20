#' Data collection year
#'
#' @param dat a dataframe: data_collection_year
#'
#' @return A ggplot object.
#' @export
dataCollectionYear <- function(dat = data_collection_year) {
  graph <- dat |>
    ggplot(aes(x = year, y = n)) +
    geom_col(fill = "#0032A0") +
    labs(x = "Year of Publication", y = "Number of Studies") +
    scale_x_continuous(breaks = ~ round(seq(min(.x), max(.x))))

  return(graph)
}
