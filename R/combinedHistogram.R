#' @title build combined histogram of measures
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import tidyr
#' @import tibble
#' @export

combinedHistogram <- function() {
  dl <- getDataLong()
  md <- getMetadata()

  condition_ids <- unique(dl$condition_id)

  fulld <- dl |>
    left_join(md)

  quest <- fulld |>
    filter(
      measure %in%
        c("stais", "stait", "neo-ffi", "asi", "promis", "ius", "pswq")
    ) |>
    select(condition_id, participant_id, measure) |>
    unique() |>
    mutate(type = "questionnaire")

  phys <- fulld |>
    filter(measure %in% c("scr", "scl", "ps", "fps", "hr")) |>
    select(condition_id, participant_id, measure) |>
    unique() |>
    mutate(type = "physiological")

  rate <- fulld |>
    filter(
      measure %in% c("anx", "arous", "aware", "expect", "fear", "val", "stress")
    ) |>
    select(condition_id, participant_id, measure) |>
    unique() |>
    mutate(type = "ratings")

  measure_cat <- rbind(quest, phys, rate) |>
    mutate(
      type = factor(
        type,
        levels = c("questionnaire", "ratings", "physiological")
      )
    ) |>
    group_by(measure, type) |>
    summarise(used = n()) |>
    arrange(type, desc(used)) |>
    group_by(type) |>
    mutate(measure = forcats::fct_reorder(measure, used))

  bp <- measure_cat |>
    ggplot(aes(x = measure, y = used, fill = type)) +
    geom_col() +
    shadowtext::geom_shadowtext(
      aes(label = used, y = 5),
      hjust = 0,
      color = "black",
      fontface = 'bold',
      bg.color = "white"
    ) +
    theme_minimal() +
    coord_flip() +
    theme_void() +
    theme(axis.text.x = element_text(size = rel(1)), legend.position = "top")
  #bp

  # test <- rbind(quest, phys, rate) |>
  #     mutate(type = factor(type, levels = c("questionnaire", "ratings", "physiological"))) |>
  #     group_by(condition_id, measure, type) |>
  #     summarise(n = n())
  # test

  # x <- rbind(quest, phys, rate) |>
  #     mutate(type = factor(type, levels = c("questionnaire", "ratings", "physiological"))) |>
  #     pivot_wider(names_from = condition_id, values_from = participant_id, values_fn = length, values_fill = 0) |>
  #     select(-measure, -type) |>
  #     as.matrix()

  x <- rbind(quest, phys, rate) |>
    select(-type) |>
    pivot_wider(
      names_from = measure,
      values_from = participant_id,
      values_fn = length,
      values_fill = 0
    ) |>
    select(-condition_id) |>
    as.matrix()

  crosstable <- t(x > 0) %*% x
  crosstable <- as_tibble(crosstable)
  colnames(crosstable) <- colnames(x)
  crosstable$measure <- colnames(x)
  crosstable <- crosstable |>
    rowwise() |>
    mutate(across(ius:aware, ~ ifelse(measure == cur_column(), NA, .)))

  hm <- crosstable %>%
    pivot_longer(
      cols = -measure,
      names_to = "measure2",
      values_to = "value"
    ) %>%
    left_join(measure_cat, by = c("measure")) %>%
    left_join(measure_cat, by = c("measure2" = "measure")) %>%
    group_by(type.x, type.y) %>%
    mutate(measure = forcats::fct_reorder(measure, used.x)) %>%
    mutate(measure2 = forcats::fct_reorder(measure2, used.y)) %>%
    ggplot(aes(x = measure, y = measure2, fill = value)) +
    geom_tile() +
    scale_y_discrete(position = "right") +
    shadowtext::geom_shadowtext(
      aes(label = value),
      color = "black",
      fontface = 'bold',
      nudge_x = 0,
      nudge_y = 0,
      bg.color = "white"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank()
    )

  graph <- egg::ggarrange(hm, bp, ncol = 2, widths = c(1, 0.5), draw = FALSE)
  return(graph)
}
