#' @title build combined histogram of measures
#' @export

phasesHistogram <- function() {
    library(dplyr)
    library(ggplot2)
    library(purrr)
    library(tidyr)
    library(tibble)


    data_long <- getDataLong()
    metadat <- getMetadata()

    study_ids <- unique(data_long$study_id)

    dat <- data_long |>
        left_join(metadat, by = c("study_id" = "study_id"))

    phases <- data_long |>
        select(study_id, participant_id, phase) |>
        unique() |> 
        drop_na() |>
        group_by(study_id, phase) |>
        summarize(n = n()) |> 
        pivot_wider(names_from = study_id, values_from = n, values_fill = 0)

    phase_names <- phases$phase
    x <- phases |>
        select(-phase) |>
        as.matrix()
    
    crosstable <- x %*% t(x > 0)
    crosstable <- as_tibble(crosstable)
    colnames(crosstable) <- phase_names
    crosstable$phase <- phase_names
    # crosstable <- crosstable |>
    #     rowwise() |>
    #     mutate(across(-phase, ~ ifelse(phase == cur_column(), NA, .)))

    hm <- crosstable %>%
        pivot_longer(cols = -phase, names_to = "phase2", values_to = "value") %>%
        # mutate(phase = forcats::fct_reorder(phase, used.x)) %>%
        # mutate(phase2 = forcats::fct_reorder(phase2, used.y)) %>%
        ggplot(aes(x = phase, y = phase2, fill = value)) +
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

    return(hm)
}
# todos:
## in der matrix phys rating und quest klar machen

# descriptive visualisierungen:
## male female verteilung
## studie größen verteilung

# welche reiforcementrates