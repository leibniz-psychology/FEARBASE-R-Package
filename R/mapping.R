############################################################
# 1) Create a private internal environment for the package
############################################################

# Create a new environment that:
# - Has no parent (emptyenv())
# - Does NOT inherit from global environment
# - Acts as a private cache for internal objects
# - Prevents namespace pollution
# - Avoids accidental overwriting by users
#
# This environment will store the "mapping" object
# once it is loaded the first time (lazy caching).
.fearbase_env <- new.env(parent = emptyenv())



############################################################
# 2) Normalize mapping ID columns
############################################################

# This function ensures that all relevant ID columns
# are stored as character vectors.
#
# Why?
# - IDs should never be numeric (leading zeros lost)
# - Prevents factor issues
# - Prevents mismatches during joins
# - Ensures stable type consistency
.normalize_mapping <- function(mapping) {

  # Define columns that must always be character
  id_columns <- c(
    "condition_id",
    "study_id",
    "paper_cond_id",
    "paper_study_id"
  )

  # Use mutate with across()
  # any_of() ensures no error if a column is missing
  # as.character ensures consistent type
  mapping |>
    mutate(across(any_of(id_columns), as.character))
}



############################################################
# 3) Attempt to load mapping from sysdata.rda
############################################################

# This function tries to load a saved mapping object
# from the package's internal R/sysdata.rda file.
#
# Important:
# - It loads into a temporary environment
# - It does NOT pollute global or namespace
# - It safely checks if the object exists
.load_mapping_from_sysdata <- function() {

  # Define possible file locations
  candidate_paths <- c(
    file.path("R", "sysdata.rda"),
    file.path(getwd(), "R", "sysdata.rda")
  )

  # Iterate over possible locations
  for (path in candidate_paths) {

    # Skip if file does not exist
    if (!file.exists(path)) {
      next
    }

    # Create temporary isolated environment
    # to avoid accidental overwriting
    tmp_env <- new.env(parent = emptyenv())

    # Load file contents into tmp_env
    load(path, envir = tmp_env)

    # Check whether object named "mapping" exists
    if (exists("mapping", envir = tmp_env, inherits = FALSE)) {

      # If yes, return that object
      return(get("mapping", envir = tmp_env, inherits = FALSE))
    }
  }

  # If nothing found, return NULL
  return(NULL)
}



############################################################
# 4) Try to retrieve mapping from package namespace
############################################################

# This function checks if the mapping object
# is already stored in the installed package namespace.
#
# This happens if sysdata.rda was bundled at build time.
.get_namespace_mapping <- function() {

  # Safely determine current package name
  pkg_name <- tryCatch(
    utils::packageName(),
    error = function(...) NULL
  )

  # If no package context exists, return NULL
  if (is.null(pkg_name) || identical(pkg_name, "")) {
    return(NULL)
  }

  # Get namespace environment of package
  ns <- asNamespace(pkg_name)

  # Check if object exists inside namespace
  if (!exists("mapping", envir = ns, inherits = FALSE)) {
    return(NULL)
  }

  # Retrieve and return mapping object
  get("mapping", envir = ns, inherits = FALSE)
}



############################################################
# 5) Core resolver: Determine correct mapping source
############################################################

# This is the central orchestration function.
#
# It implements a priority-based resolution strategy:
#
# 1) User-supplied mapping argument
# 2) Cached internal mapping (.fearbase_env)
# 3) mapping in .GlobalEnv
# 4) mapping in package namespace
# 5) mapping from sysdata.rda
# 6) Stop with error if none found
#
# It also normalizes and optionally assigns globally.
.get_mapping <- function(mapping = NULL, assign_global = FALSE) {

  ##########################################################
  # 1) User explicitly supplied mapping
  ##########################################################
  if (!is.null(mapping)) {
    return(.normalize_mapping(mapping))
  }

  ##########################################################
  # 2) Check internal cached version
  ##########################################################
  if (exists("mapping", envir = .fearbase_env, inherits = FALSE)) {
    return(.normalize_mapping(
      get("mapping", envir = .fearbase_env)
    ))
  }

  ##########################################################
  # 3) Check global environment (backwards compatibility)
  ##########################################################
  if (exists("mapping", envir = .GlobalEnv, inherits = FALSE)) {
    return(.normalize_mapping(
      get("mapping", envir = .GlobalEnv)
    ))
  }

  ##########################################################
  # 4) Check installed package namespace
  ##########################################################
  namespace_mapping <- .get_namespace_mapping()
  if (!is.null(namespace_mapping)) {
    return(.normalize_mapping(namespace_mapping))
  }

  ##########################################################
  # 5) Attempt loading from sysdata.rda
  ##########################################################
  package_mapping <- .load_mapping_from_sysdata()

  # If still NULL -> fail hard
  if (is.null(package_mapping)) {
    stop(
      "No internal mapping object could be found. ",
      "Rebuild the package mapping via data-raw/build-mapping.R."
    )
  }

  # Normalize ID types
  package_mapping <- .normalize_mapping(package_mapping)

  # Cache internally for future calls
  assign("mapping", package_mapping, envir = .fearbase_env)

  # Optionally assign to global environment
  if (isTRUE(assign_global)) {
    assign("mapping", package_mapping, envir = .GlobalEnv)
  }

  return(package_mapping)
}



############################################################
# 6) Public user-facing function
############################################################

#' Load the integrated study-to-condition mapping
#'
#' @param assign_global Logical. Should mapping also be assigned
#'   to global environment?
#'
#' @return A normalized mapping data frame.
#' @export
updateMapping <- function(assign_global = TRUE) {
  .get_mapping(assign_global = assign_global)
}



############################################################
# 7) Apply mapping to long-format data
############################################################

# This function ensures that condition_id and study_id
# are properly mapped in long-format datasets.
.apply_mapping_to_long_data <- function(dl, mapping = NULL) {

  # If condition_id already exists and contains values,
  # do nothing (idempotent behavior).
  if ("condition_id" %in% names(dl) &&
      any(!is.na(dl$condition_id))) {
    return(dl)
  }

  # Retrieve mapping (lazy-loaded)
  mapping <- .get_mapping(mapping)

  dl |>
    mutate(study_id = as.character(study_id)) |>

    # Join: study_id in dl actually equals condition_id
    left_join(
      select(mapping,
             condition_id,
             mapped_study_id = study_id),
      by = c("study_id" = "condition_id")
    ) |>

    # Reconstruct identifiers
    mutate(
      condition_id = .data$study_id,
      study_id = coalesce(
        .data$mapped_study_id,
        .data$study_id
      )
    ) |>

    # Improve column ordering
    relocate(condition_id, .before = study_id) |>

    # Remove temporary join column
    select(-mapped_study_id)
}



############################################################
# 8) Apply mapping to metadata
############################################################

# Metadata uses "id" instead of "study_id".
.apply_mapping_to_metadata <- function(md, mapping = NULL) {

  # If mapping already applied, return as-is
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
      select(mapping,
             condition_id,
             study_id),
      by = c("id" = "condition_id")
    ) |>

    mutate(
      condition_id = .data$id,
      study_id = coalesce(
        .data$study_id,
        .data$id
      )
    ) |>

    relocate(condition_id, .before = study_id)
}



############################################################
# 9) Apply mapping to study design data
############################################################

# Nearly identical to long-data version,
# adapted to study design structure.
.apply_mapping_to_study_design <- function(sd, mapping = NULL) {

  if ("condition_id" %in% names(sd) &&
      any(!is.na(sd$condition_id))) {
    return(sd)
  }

  mapping <- .get_mapping(mapping)

  sd |>
    mutate(study_id = as.character(study_id)) |>

    left_join(
      select(mapping,
             condition_id,
             mapped_study_id = study_id),
      by = c("study_id" = "condition_id")
    ) |>

    mutate(
      condition_id = .data$study_id,
      study_id = coalesce(
        .data$mapped_study_id,
        .data$study_id
      )
    ) |>

    relocate(condition_id, .before = study_id) |>

    select(-mapped_study_id)
}
