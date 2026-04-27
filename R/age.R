#' Age distribution
#'
#' @param dat The age data table produced by \code{prepareAgeData}.
#' @param type The type of plot to produce. Either "histogram", "hist", "h" or "ridge", "density", "r", "d".
#' @param grouping_variable A string specifying the variable to group by (allowed values: "condition_id", "study_id", "paper_cond_id", or "paper_study_id").
#'
#' @return A ggplot object.
#' @export
age <- function(
  dat = data_age,
  type = "histogram",
  grouping_variable = "study_id"
) {
  study_order <- dat |>
    group_by(.data[[grouping_variable]]) |>
    summarise(mean_age = median(age)) |>
    arrange(desc(mean_age)) |>
    pull(.data[[grouping_variable]])

  if (tolower(type) %in% c("histogram", "hist", "h")) {
    dat <- dat |>
      group_by(age, .data[[grouping_variable]]) |>
      summarise(n = n())

    dat[[grouping_variable]] <- factor(
      dat[[grouping_variable]],
      levels = study_order
    )

    graph <- dat |>
      ggplot(aes(x = age, y = n)) +
      geom_bar(
        aes(fill = .data[[grouping_variable]]),
        stat = "identity",
        color = "white",
        linewidth = .2
      ) +
      labs(x = "Age", y = "Number of Participants", fill = "Study ID")
  } else if (tolower(type) %in% c("ridge", "density", "r", "d")) {
    dat[[grouping_variable]] <- factor(
      dat[[grouping_variable]],
      levels = study_order
    )

    graph <- dat |>
      ggplot(aes(
        x = age,
        y = .data[[grouping_variable]],
        group = .data[[grouping_variable]],
        fill = .data[[grouping_variable]]
      )) +
      ggridges::geom_density_ridges() +
      labs(x = "Age", y = "Study ID", fill = "Study ID") +
      theme(legend.position = "none")
  } else {
    stop("unknown argument type")
  }
  return(graph)
}

#' Age descriptives
#'
#' @param dl The data in long format.
#'
#' @return A data frame with mean, sd, min and max age.
#' @export
ageDescriptives <- function(dl = data_long, grouping_variable = "study_id") {
  age <- dl |>
    filter(measure == "age") |>
    select(.data[[grouping_variable]], participant_id, value, measure) |>
    mutate(
      age = as.numeric(value),
      !!grouping_variable := as.factor(.data[[grouping_variable]])
    ) |>
    filter(!is.na(age)) |>
    summarise(
      mean_age = mean(age),
      sd = sd(age),
      min = min(age),
      max = max(age)
    )

  return(age)
}
