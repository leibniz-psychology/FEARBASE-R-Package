test_that("sex pie chart works", {
  sex() |> testthat::expect_s3_class("ggplot")
})

test_that("sex separates NA values from not reported values", {
  dl <- data.frame(
    study_id = rep("study-1", 6),
    participant_id = c(1, 2, 3, 4, 5, 6),
    measure = c("sex", "sex", "age", "sex", "sex", "sex"),
    value = c("m", "f", "32", NA, "NA", "not reported"),
    stringsAsFactors = FALSE
  )

  graph <- sex(dl)

  testthat::expect_equal(
    as.character(graph$data$sex),
    c("m", "f", "not reported", "NA")
  )
  testthat::expect_equal(graph$data$n, c(1L, 1L, 2L, 2L))
  testthat::expect_equal(
    as.character(graph$data$sex_label),
    c("male", "female", "not reported", "NA")
  )
})

test_that("sex uses repelled labels for missingness categories", {
  dl <- data.frame(
    study_id = rep("study-1", 4),
    participant_id = c(1, 2, 3, 4),
    measure = c("sex", "sex", "sex", "sex"),
    value = c("m", "f", NA, "not reported"),
    stringsAsFactors = FALSE
  )

  graph <- sex(dl)
  layer_labels <- lapply(
    graph$layers,
    function(layer) {
      layer$data$label
    }
  )

  testthat::expect_equal(layer_labels[[2]], c("male (1)", "female (1)"))
  testthat::expect_equal(layer_labels[[3]], c("not reported (1)", "NA (1)"))
  testthat::expect_s3_class(graph$layers[[3]]$geom, "GeomLabelRepel")
})

test_that("sex maps fill colours from darkest to lightest", {
  dl <- data.frame(
    study_id = rep("study-1", 4),
    participant_id = c(1, 2, 3, 4),
    measure = c("sex", "sex", "sex", "sex"),
    value = c("m", "f", "not reported", NA),
    stringsAsFactors = FALSE
  )

  graph <- sex(dl)
  built_graph <- ggplot2::ggplot_build(graph)

  testthat::expect_equal(
    built_graph$data[[1]]$fill,
    rev(fearbase_palette_v2[seq_len(4L)])
  )
})
