#' Plot the Number of Studies by Data Collection Year
#'
#' @description
#' Creates a bar plot showing how many studies in the metadata were collected
#' in each calendar year. Metadata are first passed through the package-internal
#' study-to-condition mapping so that legacy and current metadata schemas are
#' handled consistently.
#'
#' @param md A data frame containing study metadata. The data frame must contain
#'   a `year` column. If `NULL`, the function first attempts to use an object
#'   named `metadata` from the calling environment and then falls back to the
#'   package-bundled `data/metadata.csv` file.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Applies `.apply_mapping_to_metadata()` to `md`.
#'   \item Validates that a `year` column is present.
#'   \item Coerces non-missing `year` values to numeric values.
#'   \item Counts studies per year after removing missing year values.
#'   \item Returns a `ggplot2` column chart.
#' }
#'
#' Non-missing values in `year` must be numeric or coercible to numeric. Missing
#' values are excluded from the plot. The function raises an error if no valid
#' year values remain after validation.
#'
#' @return A `ggplot2` object showing one column per data collection year.
#'
#' @examples
#' \dontrun{
#' dataCollectionYear(metadata)
#' }
#'
#' @export
dataCollectionYear <- function(md = NULL) {
  if (is.null(md)) {
    if (exists("metadata", envir = parent.frame(), inherits = TRUE)) {
      md <- get("metadata", envir = parent.frame(), inherits = TRUE)
    } else {
      metadata_path <- system.file(
        "data",
        "metadata.csv",
        package = "fearbase",
        mustWork = FALSE
      )
    }

    if (is.null(md) && identical(metadata_path, "")) {
      stop(
        "`md` must be supplied, an object named `metadata` must exist ",
        "in the calling environment, or bundled metadata must be available.",
        call. = FALSE
      )
    }

    if (is.null(md)) {
      # Use the package-bundled metadata only as a final fallback so explicit
      # user input remains the primary and reproducible data source.
      md <- readr::read_csv(metadata_path, show_col_types = FALSE)
    }
  }

  if (!is.data.frame(md)) {
    stop("`md` must be a data frame.", call. = FALSE)
  }

  md <- .apply_mapping_to_metadata(md)

  if (!"year" %in% names(md)) {
    stop("`md` must contain a `year` column.", call. = FALSE)
  }

  # Coerce years explicitly so the downstream scale receives numeric values and
  # malformed, non-missing years fail with a clear input-validation error.
  raw_year <- md$year

  if (is.numeric(raw_year)) {
    year <- as.numeric(raw_year)
  } else {
    year <- suppressWarnings(as.numeric(as.character(raw_year)))
  }
  invalid_year <- !is.na(raw_year) & is.na(year)

  if (any(invalid_year)) {
    stop(
      "All non-missing values in `md$year` must be numeric or coercible ",
      "to numeric.",
      call. = FALSE
    )
  }

  md$year <- year

  valid_year <- md$year[!is.na(md$year)]

  if (length(valid_year) == 0L) {
    stop("`md$year` must contain at least one non-missing value.", call. = FALSE)
  }

  # Count observations with base R to keep this helper independent from
  # package-level imports while still returning a regular plotting data frame.
  data_collection_year <- as.data.frame(table(valid_year))
  names(data_collection_year) <- c("year", "n")
  data_collection_year$year <- as.numeric(as.character(data_collection_year$year))
  data_collection_year$n <- as.integer(data_collection_year$n)

  year_breaks <- seq(
    from = floor(min(data_collection_year$year)),
    to = ceiling(max(data_collection_year$year)),
    by = 1
  )

  graph <- data_collection_year |>
    ggplot(
      aes(
        x = !!rlang::sym("year"),
        y = !!rlang::sym("n")
      )
    ) +
    geom_col(fill = "#0032A0") +
    labs(x = "Year of Publication", y = "Number of Studies") +
    scale_x_continuous(breaks = year_breaks)

  return(graph)
}


# TODO: number of studies vs. number of conditions?
# TODO: dynamic plot y axis title ("Number of Studies" vs. "Number of Datasets"?)
