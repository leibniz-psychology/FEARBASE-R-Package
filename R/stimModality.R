#' @title sex distribution
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

stimModality <- function(type = "us") {

    md <- getMetadata()

    if (tolower(type) %in% c("us", "unconditioned stimulus", "unconditioned stimuli")) {
    graph <- md |>
        select(condition_id, study_id, us_type, cs_type) |>
        ggplot(aes(x = us_type)) +
            geom_bar()
    } else if (tolower(type) %in% c("cs", "conditioned stimulus", "conditioned stimuli")){
    graph <- md |>
        select(condition_id, study_id, us_type, cs_type) |>
        ggplot(aes(x = cs_type)) +
            geom_bar()
    } else {
       stop("unknown stimulus type")
    }

    return(graph)
}