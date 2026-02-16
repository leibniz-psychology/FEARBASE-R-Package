#' @title data collection year
#' @export

dataCollectionYear <- function() {
    library(dplyr)
    library(ggplot2)
    library(tidyr)

    mdat <- getMetadata()

    graph <- mdat |>
        select(year) |>
        table() |> as_tibble() |>
        mutate(year = as.numeric(year)) |>
        ggplot(aes(x = year, y=n)) +
        geom_segment(aes(x=year, xend=year, y=0, yend=n), color = "#0032A0", lineend = "round", linewidth = 3) +
        geom_point(color = "#FFC94F", size = 8) +
        labs(x = "Year of Publication", y = "Number of Studies") +
        scale_x_continuous(breaks = ~ round(seq(min(.x), max(.x))))

    return(graph)
}