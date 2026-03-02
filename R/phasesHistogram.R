#' @title build combined histogram of measures
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import tidyr
#' @import tibble
#' @export

phasesHistogram <- function() {
    data_long <- getDataLong()
    metadat <- getMetadata()

    condition_ids <- unique(data_long$condition_id)

    phases <- data_long |>
        select(condition_id, participant_id, phase) |>
        unique() |> 
        drop_na() |>
        group_by(condition_id, phase) |>
        summarize(n = n()) |> 
        pivot_wider(names_from = condition_id, values_from = n, values_fill = 0)

    bp <- phases |>
        pivot_longer(cols = -phase, names_to = "condition_id", values_to = "used") |>
        group_by(phase) |>
        summarise(used = sum(used)) |>
        ggplot(aes(x = phase, y = used)) +
        geom_col() +
        geom_text(aes(label = used, y = 50), color = "black") +
        theme_minimal() +
        coord_flip() +
        scale_fill_manual(values = c("gray", "#6a95f1", "#FFC94F")) +
        theme_void() +
        theme(axis.text.x = element_text(size = rel(1)), legend.position = "top")

    phase_names <- phases$phase

    x <- phases |>
        select(-phase) |>
        as.matrix()
    
    # possibly replace the matrix multiplication solution with corssing() or combn()
    # to get all pairwise combinations of phases and count the number of participants that have both phases
    crosstable <- x %*% t(x > 0)
    crosstable <- as_tibble(crosstable)
    colnames(crosstable) <- phase_names
    crosstable$phase <- phase_names
    # crosstable <- crosstable |>
    #     rowwise() |>
    #     mutate(across(-phase, ~ ifelse(phase == cur_column(), NA, .)))

    hm <- crosstable |>
        pivot_longer(cols = -phase, names_to = "phase2", values_to = "value") |>
        mutate(highlight = factor(ifelse(phase == phase2, 1, 0))) |>
        # mutate(phase = forcats::fct_reorder(phase, used.x)) %>%
        # mutate(phase2 = forcats::fct_reorder(phase2, used.y)) %>%
        ggplot(aes(x = phase, y = phase2, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "#6a95f1", high = "#FFC94F") +
        # scale_color_manual(values = c("#ffffff00", "red")) +
        scale_y_discrete(position = "right") +
        geom_text(aes(label = value), color = "black", nudge_x = 0, nudge_y = 0) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.title = element_blank()
        )

    graph <- egg::ggarrange(hm, bp, ncol = 2, widths = c(1, 0.5))

    return(graph)
}