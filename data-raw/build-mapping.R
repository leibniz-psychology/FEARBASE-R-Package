required_columns <- c(
  "condition_id",
  "study_id",
  "paper_cond_id",
  "paper_study_id"
)

mapping_path <- file.path("data-raw", "mapping.csv")
output_path <- file.path("R", "sysdata.rda")

if (!file.exists(mapping_path)) {
  stop("Mapping source file not found: ", mapping_path)
}

mapping <- readr::read_csv(mapping_path, show_col_types = FALSE)

missing_columns <- setdiff(required_columns, names(mapping))
if (length(missing_columns) > 0) {
  stop(
    "The mapping source is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

mapping <- mapping |>
  dplyr::mutate(dplyr::across(dplyr::any_of(required_columns), as.character))

if (anyDuplicated(mapping$condition_id) > 0) {
  stop("The mapping contains duplicated condition_id values.")
}

save(mapping, file = output_path, compress = "xz")
