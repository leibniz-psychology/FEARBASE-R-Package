#' Age distribution
#'
#' @param dl The data in long format
#' @param type The type of plot to produce. Either "histogram", "hist", "h" or "ridge", "density", "r", "d".
#' @param grouping_variable A string specifying the variable to group by (allowed values: "condition_id", "study_id", "paper_cond_id", or "paper_study_id").
#'
#' @return A ggplot object.
#' @export
age <- function(
  dl,
  type = "histogram",
  grouping_variable = "study_id"
) {
  dl <- .apply_mapping_to_long_data(dl)

  # Process Data
  data_age <- dl |>
    filter(measure == "age") |>
    select(
      any_of(c(
        "condition_id",
        "study_id",
        "paper_cond_id",
        "paper_study_id"
      )),
      participant_id,
      value,
      measure
    ) |>
    mutate(
      age = as.numeric(value),
      across(any_of(c(
        "condition_id",
        "study_id",
        "paper_cond_id",
        "paper_study_id"
      )), as.factor)
    ) |>
    filter(!is.na(age))

  # Plot
  study_order <- data_age |>
    group_by(.data[[grouping_variable]]) |>
    summarise(mean_age = median(age)) |>
    arrange(desc(mean_age)) |>
    pull(.data[[grouping_variable]])

  if (tolower(type) %in% c("histogram", "hist", "h")) {
    data_age <- data_age |>
      group_by(age, .data[[grouping_variable]]) |>
      summarise(n = n())

    data_age[[grouping_variable]] <- factor(
      data_age[[grouping_variable]],
      levels = study_order
    )

    graph <- data_age |>
      ggplot(aes(x = age, y = n)) +
      geom_bar(
        aes(fill = .data[[grouping_variable]]),
        stat = "identity",
        color = "white",
        linewidth = .2
      ) +
      labs(x = "Age", y = "Number of Participants", fill = "Study ID")
  } else if (tolower(type) %in% c("ridge", "density", "r", "d")) {
    data_age[[grouping_variable]] <- factor(
      data_age[[grouping_variable]],
      levels = study_order
    )

    graph <- data_age |>
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
ageDescriptives <- function(dl, grouping_variable = "study_id") {
  dl <- .apply_mapping_to_long_data(dl)

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
