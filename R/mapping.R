.fearbase_env <- new.env(parent = emptyenv())

.normalize_mapping <- function(mapping) {
  id_columns <- c("condition_id", "study_id", "paper_cond_id", "paper_study_id")

  mapping |>
    mutate(across(any_of(id_columns), as.character))
}

.load_mapping_from_sysdata <- function() {
  candidate_paths <- c(
    file.path("R", "sysdata.rda"),
    file.path(getwd(), "R", "sysdata.rda")
  )

  for (path in candidate_paths) {
    if (!file.exists(path)) {
      next
    }

    tmp_env <- new.env(parent = emptyenv())
    load(path, envir = tmp_env)

    if (exists("mapping", envir = tmp_env, inherits = FALSE)) {
      return(get("mapping", envir = tmp_env, inherits = FALSE))
    }
  }

  return(NULL)
}

.get_namespace_mapping <- function() {
  pkg_name <- tryCatch(utils::packageName(), error = function(...) NULL)

  if (is.null(pkg_name) || identical(pkg_name, "")) {
    return(NULL)
  }

  ns <- asNamespace(pkg_name)

  if (!exists("mapping", envir = ns, inherits = FALSE)) {
    return(NULL)
  }

  get("mapping", envir = ns, inherits = FALSE)
}

.get_mapping <- function(mapping = NULL, assign_global = FALSE) {
  if (!is.null(mapping)) {
    return(.normalize_mapping(mapping))
  }

  if (exists("mapping", envir = .fearbase_env, inherits = FALSE)) {
    return(.normalize_mapping(get("mapping", envir = .fearbase_env)))
  }

  if (exists("mapping", envir = .GlobalEnv, inherits = FALSE)) {
    return(.normalize_mapping(get("mapping", envir = .GlobalEnv)))
  }

  namespace_mapping <- .get_namespace_mapping()
  if (!is.null(namespace_mapping)) {
    return(.normalize_mapping(namespace_mapping))
  }

  package_mapping <- .load_mapping_from_sysdata()

  if (is.null(package_mapping)) {
    stop(
      "No internal mapping object could be found. ",
      "Rebuild the package mapping via data-raw/build-mapping.R."
    )
  }

  package_mapping <- .normalize_mapping(package_mapping)
  assign("mapping", package_mapping, envir = .fearbase_env)

  if (isTRUE(assign_global)) {
    assign("mapping", package_mapping, envir = .GlobalEnv)
  }

  return(package_mapping)
}

#' Load the integrated study-to-condition mapping
#'
#' @param assign_global Whether the package-internal mapping should also be
#'   assigned to the global environment for backwards compatibility.
#'
#' @return A data frame with the mapping between `condition_id` and `study_id`.
#' @export
updateMapping <- function(assign_global = TRUE) {
  .get_mapping(assign_global = assign_global)
}

.apply_mapping_to_long_data <- function(dl, mapping = NULL) {
  if ("condition_id" %in% names(dl) && any(!is.na(dl$condition_id))) {
    return(dl)
  }

  mapping <- .get_mapping(mapping)

  dl |>
    mutate(study_id = as.character(study_id)) |>
    left_join(
      select(mapping, condition_id, mapped_study_id = study_id),
      by = c("study_id" = "condition_id")
    ) |>
    mutate(
      condition_id = .data$study_id,
      study_id = dplyr::coalesce(.data$mapped_study_id, .data$study_id)
    ) |>
    relocate(condition_id, .before = study_id) |>
    select(-mapped_study_id)
}

.apply_mapping_to_metadata <- function(md, mapping = NULL) {
  if (
    all(c("condition_id", "study_id") %in% names(md)) &&
      any(!is.na(md$condition_id))
  ) {
    return(md)
  }

  mapping <- .get_mapping(mapping)

  md |>
    mutate(id = as.character(id)) |>
    left_join(
      select(mapping, condition_id, study_id),
      by = c("id" = "condition_id")
    ) |>
    mutate(
      condition_id = .data$id,
      study_id = dplyr::coalesce(.data$study_id, .data$id)
    ) |>
    relocate(condition_id, .before = study_id)
}

.apply_mapping_to_study_design <- function(sd, mapping = NULL) {
  if ("condition_id" %in% names(sd) && any(!is.na(sd$condition_id))) {
    return(sd)
  }

  mapping <- .get_mapping(mapping)

  sd |>
    mutate(study_id = as.character(study_id)) |>
    left_join(
      select(mapping, condition_id, mapped_study_id = study_id),
      by = c("study_id" = "condition_id")
    ) |>
    mutate(
      condition_id = .data$study_id,
      study_id = dplyr::coalesce(.data$mapped_study_id, .data$study_id)
    ) |>
    relocate(condition_id, .before = study_id) |>
    select(-mapped_study_id)
}
