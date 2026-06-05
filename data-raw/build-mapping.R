required_columns <- c(
  "condition_id",
  "study_id",
  "paper_cond_id",
  "paper_study_id"
)

mapping_path <- file.path("data-raw", "mapping.csv")

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

# Store the prepared table under a dot-prefixed name because the object is only
# meant for package internals. usethis writes it to R/sysdata.rda, which R then
# lazy-loads into the package namespace when installed or loaded with load_all().
.mapping <- mapping
usethis::use_data(
  .mapping,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)
