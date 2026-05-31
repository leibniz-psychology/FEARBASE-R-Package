test_that("mapping helper leaves current long-data schemas unchanged", {
  dl <- fixture_long_data()

  mapped <- fearbase:::.apply_mapping_to_long_data(dl)

  testthat::expect_identical(mapped, dl)
})

test_that("mapping helper maps legacy long-data identifiers with explicit mapping", {
  dl <- tibble::tibble(
    study_id = "c1",
    participant_id = "p1",
    measure = "age",
    value = "21"
  )
  mapping <- tibble::tibble(
    condition_id = "c1",
    study_id = "s1",
    paper_cond_id = "pc1",
    paper_study_id = "ps1"
  )

  mapped <- fearbase:::.apply_mapping_to_long_data(dl, mapping = mapping)

  testthat::expect_equal(mapped$condition_id, "c1")
  testthat::expect_equal(mapped$study_id, "s1")
})

test_that("metadata mapping reports missing source identifiers clearly", {
  md <- tibble::tibble(year = 2020)

  testthat::expect_error(
    fearbase:::.apply_mapping_to_metadata(md, mapping = tibble::tibble()),
    "Missing required column"
  )
})
