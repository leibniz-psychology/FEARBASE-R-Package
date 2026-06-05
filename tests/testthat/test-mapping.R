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
  if (exists(".mapping", envir = cache_env, inherits = FALSE)) {
    old_mapping <- get(".mapping", envir = cache_env, inherits = FALSE)
    on.exit(assign(".mapping", old_mapping, envir = cache_env), add = TRUE)
  } else {
    on.exit(
      if (exists(".mapping", envir = cache_env, inherits = FALSE)) {
        rm(".mapping", envir = cache_env)
      },
      add = TRUE
    )
  }

  assign(
    ".mapping",
    tibble::tibble(condition_id = 1, study_id = 2),
    envir = cache_env
  )

  resolved <- fearbase:::.get_mapping()

  testthat::expect_equal(resolved$condition_id, "1")
  testthat::expect_equal(resolved$study_id, "2")
})

test_that("mapping resolver uses bundled internal data", {
  cache_env <- fearbase:::.fearbase_env
  if (exists(".mapping", envir = cache_env, inherits = FALSE)) {
    old_mapping <- get(".mapping", envir = cache_env, inherits = FALSE)
    on.exit(assign(".mapping", old_mapping, envir = cache_env), add = TRUE)
    rm(".mapping", envir = cache_env)
  } else {
    on.exit(
      if (exists(".mapping", envir = cache_env, inherits = FALSE)) {
        rm(".mapping", envir = cache_env)
      },
      add = TRUE
    )
  }

  resolved <- fearbase:::.get_mapping()

  testthat::expect_s3_class(resolved, "data.frame")
  testthat::expect_named(
    resolved,
    c("condition_id", "study_id", "paper_cond_id", "paper_study_id")
  )
  testthat::expect_true(all(vapply(resolved, is.character, logical(1))))
})

test_that("mapping resolver prefers bundled internal data over global data", {
  cache_env <- fearbase:::.fearbase_env
  if (exists(".mapping", envir = cache_env, inherits = FALSE)) {
    old_cached_mapping <- get(".mapping", envir = cache_env, inherits = FALSE)
    on.exit(
      assign(".mapping", old_cached_mapping, envir = cache_env),
      add = TRUE
    )
    rm(".mapping", envir = cache_env)
  } else {
    on.exit(
      if (exists(".mapping", envir = cache_env, inherits = FALSE)) {
        rm(".mapping", envir = cache_env)
      },
      add = TRUE
    )
  }

  if (exists("mapping", envir = .GlobalEnv, inherits = FALSE)) {
    old_global_mapping <- get("mapping", envir = .GlobalEnv, inherits = FALSE)
    on.exit(
      assign("mapping", old_global_mapping, envir = .GlobalEnv),
      add = TRUE
    )
  } else {
    on.exit(rm("mapping", envir = .GlobalEnv), add = TRUE)
  }

  assign(
    "mapping",
    tibble::tibble(condition_id = "global", study_id = "global"),
    envir = .GlobalEnv
  )

  resolved <- fearbase:::.get_mapping()

  testthat::expect_false("global" %in% resolved$condition_id)
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

test_that("update_mapping returns mapping without global assignment by default", {
  if (exists("mapping", envir = .GlobalEnv, inherits = FALSE)) {
    old_global_mapping <- get("mapping", envir = .GlobalEnv, inherits = FALSE)
    on.exit(
      assign("mapping", old_global_mapping, envir = .GlobalEnv),
      add = TRUE
    )
    rm("mapping", envir = .GlobalEnv)
  } else {
    on.exit(
      if (exists("mapping", envir = .GlobalEnv, inherits = FALSE)) {
        rm("mapping", envir = .GlobalEnv)
      },
      add = TRUE
    )
  }

  resolved <- update_mapping()

  testthat::expect_s3_class(resolved, "data.frame")
  testthat::expect_false(exists("mapping", envir = .GlobalEnv, inherits = FALSE))
})
