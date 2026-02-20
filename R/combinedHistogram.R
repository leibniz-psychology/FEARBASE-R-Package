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

    study_ids <- unique(dl$study_id)

    fulld <- dl |>
        left_join(md)

    quest <- fulld |>
        filter(measure %in% c("stais", "stait", "neo-ffi", "asi", "promis", "ius", "pswq")) |>
        select(study_id, participant_id, measure) |>
        unique() |>
        mutate(type = "questionnaire")

    phys <- fulld |>
        filter(measure %in% c("scr", "scl", "ps", "fps", "hr")) |>
        select(study_id, participant_id, measure) |>
        unique() |>
        mutate(type = "physiological")

    rate <- fulld |>
        filter(measure %in% c("anx", "arous", "aware", "expect", "fear", "val", "stress")) |>
        select(study_id, participant_id, measure) |>
        unique() |>
        mutate(type = "ratings")

    measure_cat <- rbind(quest, phys, rate) |>
        mutate(type = factor(type, levels = c("questionnaire", "ratings", "physiological"))) |>
        group_by(measure, type) |>
        summarise(used = n()) |>
        arrange(type, desc(used)) |>
        group_by(type) |>
        mutate(measure = forcats::fct_reorder(measure, used))

    bp <- measure_cat |>
        ggplot(aes(x = measure, y = used, fill = type)) +
        geom_col() +
        geom_text(aes(label = used), position = position_stack(vjust = 0.5), color = "black") +
        theme_minimal() +
        coord_flip() +
        scale_fill_manual(values = c("gray", "#0033a07c", "#FFC94F")) +
        theme_void() +
        theme(axis.text.x = element_text(size = rel(1)), legend.position = "top")
    #bp

    x <- rbind(quest, phys, rate) |>
        mutate(type = factor(type, levels = c("questionnaire", "ratings", "physiological"))) |>
        pivot_wider(names_from = study_id, values_from = participant_id, values_fn = length, values_fill = 0) |>
        select(-measure, -type) |>
        as.matrix()

    x_t <- rbind(quest, phys, rate) |>
        select(-type) |>
        pivot_wider(names_from = measure, values_from = participant_id, values_fn = length, values_fill = 0) |>
        select(-study_id) |>
        as.matrix()
    
    crosstable <- x %*% t(x > 0)
    crosstable <- as_tibble(crosstable)
    colnames(crosstable) <- colnames(x_t)
    crosstable$measure <- colnames(x_t)
    crosstable <- crosstable |>
        rowwise() |>
        mutate(across(ius:aware, ~ ifelse(measure == cur_column(), NA, .)))

    hm <- crosstable %>%
        pivot_longer(cols = -measure, names_to = "measure2", values_to = "value") %>%
        left_join(measure_cat, by = c("measure")) %>%
        left_join(measure_cat, by = c("measure2" = "measure")) %>%
        group_by(type.x, type.y) %>%
        mutate(measure = forcats::fct_reorder(measure, used.x)) %>%
        mutate(measure2 = forcats::fct_reorder(measure2, used.y)) %>%
        ggplot(aes(x = measure, y = measure2, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "#0033a07c", high = "#FFC94F") +
        scale_y_discrete(position = "right") +
        geom_text(aes(label = value), color = "black", nudge_x = -0.3, nudge_y = 0.3) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.title = element_blank()
        )

    graph <- egg::ggarrange(hm, bp, ncol = 2, widths = c(1, 0.5))
    return(graph)
}
# todos:
## in der matrix phys rating und quest klar machen

# descriptive visualisierungen:
## male female verteilung
## studie größen verteilung

# welche reiforcementrates