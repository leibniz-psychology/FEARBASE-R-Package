#' @title data collection year
#' @export

dataCollectionYear <- function(md = metadata) {
  graph <- md |>
    select(year) |>
    table() |>
    as_tibble() |>
    mutate(year = as.numeric(year)) |>
    ggplot(aes(x = year, y = n)) +
    geom_col(fill = "#0032A0") +
    labs(x = "Year of Publication", y = "Number of Studies") +
    scale_x_continuous(breaks = ~ round(seq(min(.x), max(.x))))

  return(graph)
}
