fixture_country_metadata <- function() {
  tibble::tribble(
    ~id, ~condition_id, ~study_id, ~dataCountry,
    "condition-1", "condition-1", "study-1", "Germany",
    "condition-1", "condition-1", "study-1", "Germany",
    "condition-2", "condition-2", "study-2", "United States",
    "condition-3", "condition-3", "study-2", "USA",
    "condition-4", "condition-4", "study-3", NA_character_,
    NA_character_, NA_character_, "study-4", "France"
  )
}

fixture_country_long_data <- function() {
  tibble::tribble(
    ~condition_id, ~study_id, ~participant_id,
    "condition-1", "study-1", "participant-1",
    "condition-1", "study-1", "participant-1",
    "condition-1", "study-1", "participant-2",
    "condition-2", "study-2", "participant-3",
    "condition-3", "study-2", "participant-3",
    "condition-3", "study-2", "participant-4",
    "condition-4", "study-3", "participant-5"
  )
}

test_that("choropleth country preparation counts distinct datasets", {
  testthat::skip_if_not_installed("countrycode")

  country_counts <- fearbase:::.prepare_choropleth_country_counts(
    fixture_country_metadata(),
    country_variable = "dataCountry",
    dataset_id_variable = "id"
  )

  country_counts <- dplyr::arrange(country_counts, country_clean)

  testthat::expect_equal(as.character(country_counts$country_clean), c(
    "Germany",
    "United States"
  ))
  testthat::expect_equal(country_counts$n, c(1L, 2L))
  testthat::expect_match(
    country_counts$tooltip[country_counts$country_clean == "United States"],
    "Dataset IDs: condition-2, condition-3",
    fixed = TRUE
  )
})

test_that("choropleth country tooltips sort dataset IDs ascending", {
  testthat::skip_if_not_installed("countrycode")

  metadata <- data.frame(
    id = factor(
      c("condition-10", "condition-1", "condition-2"),
      levels = c("condition-10", "condition-2", "condition-1")
    ),
    condition_id = c("condition-10", "condition-1", "condition-2"),
    study_id = "study-1",
    dataCountry = "Germany"
  )

  country_counts <- fearbase:::.prepare_choropleth_country_counts(
    metadata,
    country_variable = "dataCountry",
    dataset_id_variable = "id"
  )

  testthat::expect_equal(
    country_counts$tooltip,
    "Dataset IDs: condition-1, condition-2, condition-10"
  )
  testthat::expect_equal(
    fearbase:::.sort_choropleth_dataset_ids(factor(c("10", "1", "2"))),
    c("1", "2", "10")
  )
})

test_that("choropleth country preparation counts mapped studies", {
  testthat::skip_if_not_installed("countrycode")

  country_counts <- fearbase:::.prepare_choropleth_country_counts(
    fixture_country_metadata(),
    country_variable = "dataCountry",
    dataset_id_variable = "id",
    count = "studies"
  )

  country_counts <- dplyr::arrange(country_counts, country_clean)

  testthat::expect_equal(country_counts$n, c(1L, 1L))
  testthat::expect_match(
    country_counts$tooltip[country_counts$country_clean == "United States"],
    "Dataset IDs: condition-2, condition-3",
    fixed = TRUE
  )
})

test_that("choropleth country preparation counts participants from long data", {
  testthat::skip_if_not_installed("countrycode")

  country_counts <- fearbase:::.prepare_choropleth_country_counts(
    fixture_country_metadata(),
    dl = fixture_country_long_data(),
    country_variable = "dataCountry",
    dataset_id_variable = "id",
    count = "participants"
  )

  country_counts <- dplyr::arrange(country_counts, country_clean)

  testthat::expect_equal(country_counts$n, c(2L, 2L))
  testthat::expect_match(
    country_counts$tooltip[country_counts$country_clean == "United States"],
    "Dataset IDs: condition-2, condition-3",
    fixed = TRUE
  )
})

test_that("choropleth country preparation validates input data", {
  testthat::skip_if_not_installed("countrycode")

  testthat::expect_error(
    fearbase:::.prepare_choropleth_country_counts(
      data.frame(dataCountry = "Germany"),
      country_variable = "dataCountry",
      dataset_id_variable = "id"
    ),
    "Missing required column"
  )
  testthat::expect_error(
    fearbase:::.prepare_choropleth_country_counts(
      data.frame(id = "study-1", dataCountry = NA_character_),
      country_variable = "dataCountry",
      dataset_id_variable = "id"
    ),
    "at least one non-missing country"
  )
  testthat::expect_error(
    fearbase:::.prepare_choropleth_country_counts(
      fixture_country_metadata(),
      country_variable = "dataCountry",
      dataset_id_variable = "id",
      count = "participants"
    ),
    "`dl` must be supplied"
  )
  testthat::expect_error(
    fearbase:::.prepare_choropleth_country_counts(
      fixture_country_metadata(),
      country_variable = "dataCountry",
      dataset_id_variable = "id",
      count = "invalid"
    ),
    "`count` must be one of"
  )
  testthat::expect_error(
    choropleth_country(
      fixture_country_metadata(),
      save_html_widget = NA
    ),
    "`save_html_widget` must be `TRUE` or `FALSE`"
  )
})

test_that("choropleth country count titles match requested count", {
  testthat::expect_equal(
    fearbase:::.choropleth_country_count_axis_title("datasets"),
    "Number of Datasets"
  )
  testthat::expect_equal(
    fearbase:::.choropleth_country_count_axis_title("studies"),
    "Number of Studies"
  )
  testthat::expect_equal(
    fearbase:::.choropleth_country_count_axis_title("participants"),
    "Number of Participants."
  )
})

test_that("choropleth country count breaks stay sparse for large counts", {
  testthat::expect_equal(
    fearbase:::.choropleth_country_count_breaks(4),
    0:4
  )

  large_breaks <- fearbase:::.choropleth_country_count_breaks(250)

  testthat::expect_true(0L %in% large_breaks)
  testthat::expect_lte(length(large_breaks), 8L)
  testthat::expect_true(all(large_breaks >= 0L))
  testthat::expect_true(all(large_breaks <= 250L))
})

test_that("choropleth country returns an interactive girafe widget", {
  testthat::skip_if_not_installed("countrycode")
  testthat::skip_if_not_installed("ggiraph")
  testthat::skip_if_not_installed("patchwork")
  testthat::skip_if_not_installed("rnaturalearth")
  testthat::skip_if_not_installed("sf")

  graph <- choropleth_country(
    fixture_country_metadata(),
    count = "studies",
    include_bar = FALSE,
    map_scale = "small"
  )

  testthat::expect_s3_class(graph, "girafe")
})

test_that("choropleth country can save an HTML widget", {
  testthat::skip_if_not_installed("countrycode")
  testthat::skip_if_not_installed("ggiraph")
  testthat::skip_if_not_installed("patchwork")
  testthat::skip_if_not_installed("rnaturalearth")
  testthat::skip_if_not_installed("sf")

  temporary_working_directory <- tempfile("choropleth-country-widget-")

  dir.create(temporary_working_directory)
  on.exit(unlink(temporary_working_directory, recursive = TRUE), add = TRUE)

  withr::local_dir(temporary_working_directory)

  graph <- choropleth_country(
    fixture_country_metadata(),
    include_bar = FALSE,
    map_scale = "small",
    save_html_widget = TRUE
  )

  testthat::expect_s3_class(graph, "girafe")
  testthat::expect_true(file.exists("choropleth_country.html"))
  testthat::expect_true(dir.exists("choropleth_country_files"))
})
