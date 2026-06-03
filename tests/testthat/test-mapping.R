test_that("mapping helper leaves current long-data schemas unchanged", {
  dl <- fixture_long_data()

  mapped <- fearbase:::.apply_mapping_to_long_data(dl)

  testthat::expect_identical(mapped, dl)
})

test_that("mapping helper maps legacy long-data identifiers", {
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

test_that("mapping normalization coerces identifier columns to character", {
  mapping <- tibble::tibble(
    condition_id = 1,
    study_id = 2,
    paper_cond_id = 3,
    paper_study_id = 4
  )

  normalized <- fearbase:::.normalize_mapping(mapping)

  testthat::expect_true(all(vapply(normalized, is.character, logical(1))))
})

test_that("mapping resolver returns explicit mappings", {
  mapping <- tibble::tibble(condition_id = 1, study_id = 2)

  resolved <- fearbase:::.get_mapping(mapping)

  testthat::expect_equal(resolved$condition_id, "1")
  testthat::expect_equal(resolved$study_id, "2")
})

test_that("mapping resolver uses and normalizes cached mappings", {
  cache_env <- fearbase:::.fearbase_env
  if (exists("mapping", envir = cache_env, inherits = FALSE)) {
    old_mapping <- get("mapping", envir = cache_env, inherits = FALSE)
    on.exit(assign("mapping", old_mapping, envir = cache_env), add = TRUE)
  } else {
    on.exit(rm("mapping", envir = cache_env), add = TRUE)
  }

  assign(
    "mapping",
    tibble::tibble(condition_id = 1, study_id = 2),
    envir = cache_env
  )

  resolved <- fearbase:::.get_mapping()

  testthat::expect_equal(resolved$condition_id, "1")
  testthat::expect_equal(resolved$study_id, "2")
})

test_that("mapping loader reads sysdata mappings from the working directory", {
  temporary_working_directory <- tempfile("mapping-sysdata-")
  dir.create(file.path(temporary_working_directory, "R"), recursive = TRUE)
  on.exit(unlink(temporary_working_directory, recursive = TRUE), add = TRUE)

  mapping <- tibble::tibble(condition_id = "c1", study_id = "s1")
  save(mapping, file = file.path(
    temporary_working_directory,
    "R",
    "sysdata.rda"
  ))

  withr::local_dir(temporary_working_directory)

  loaded <- fearbase:::.load_mapping_from_sysdata()

  testthat::expect_equal(loaded$condition_id, "c1")
  testthat::expect_equal(loaded$study_id, "s1")
})

test_that("metadata mapping maps legacy condition identifiers", {
  md <- tibble::tibble(id = "c1", year = 2020)
  mapping <- tibble::tibble(condition_id = "c1", study_id = "s1")

  mapped <- fearbase:::.apply_mapping_to_metadata(md, mapping = mapping)

  testthat::expect_equal(mapped$condition_id, "c1")
  testthat::expect_equal(mapped$study_id, "s1")
})

test_that("study-design mapping maps legacy condition identifiers", {
  study_design <- tibble::tibble(study_id = "c1", name = "hab")
  mapping <- tibble::tibble(condition_id = "c1", study_id = "s1")

  mapped <- fearbase:::.apply_mapping_to_study_design(
    study_design,
    mapping = mapping
  )

  testthat::expect_equal(mapped$condition_id, "c1")
  testthat::expect_equal(mapped$study_id, "s1")
})
