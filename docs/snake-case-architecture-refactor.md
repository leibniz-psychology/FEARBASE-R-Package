# Snake Case and R File Architecture Refactor

## Goal

Refactor the package so preferred public function names, internal helper names,
source file names, examples, tests, vignettes, and maintainer documentation use
tidyverse-style snake_case. Existing camelCase public entry points remain
available as deprecated compatibility wrappers.

## Milestones

- [x] Rename preferred public implementations to snake_case.
- [x] Update internal call sites, tests, vignettes, README examples, and
      maintainer documentation to use snake_case names.
- [x] Add deprecated camelCase compatibility wrappers in `R/compatibility.R`.
- [x] Reorganize `R/` files into cohesive snake_case source files.
- [x] Add architecture tests for exported names and compatibility wrappers.
- [x] Regenerate roxygen output and `NAMESPACE`.
- [x] Run package tests.
- [x] Run full package build and check with Pandoc-enabled vignettes.

## Compatibility Policy

The snake_case functions are the primary implementations and the preferred API.
CamelCase names that were previously exported remain exported wrappers for now.
Those wrappers call `.Deprecated("new_name")` and then delegate to the new
implementation so current user scripts and OpenCPU endpoints can migrate
gradually.

## File Organization Targets

- `R/co-occurrence-heatmaps.R`: measure and phase co-occurrence heatmaps plus
  their tightly-coupled plotting helpers.
- `R/validate.R`: generic internal validation helpers.
- `R/resolve-data.R`: caller-side data resolution helpers.
- `R/codebook.R`: codebook resolution and label mapping helpers.
- `R/phase.R`: phase ordering and phase display helpers.
- `R/ggplot-diagnostics.R`: ggplot debugging helper.
- `R/metadata-helpers.R`: small metadata utilities.
- `R/color.R`: color conversion utilities.
- `R/opencpu.R`: OpenCPU-oriented data transport helpers.
- `R/trial-phase-counts.R`: long-data trial count plotting and descriptives.
- `R/study-design-trial-counts.R`: study-design trial count plotting.
- Feature files use snake_case filenames, for example
  `R/data-collection-year.R`, `R/sample-size.R`, and
  `R/stimulus-modality.R`.

## Verification Commands

Use the project R installation:

```powershell
& "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe" -e "roxygen2::roxygenise('.')"
& "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe" -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"
$env:RSTUDIO_PANDOC = "C:/Program Files/Pandoc"
& "C:\Program Files\R\R-4.6.0\bin\x64\R.exe" CMD build .
& "C:\Program Files\R\R-4.6.0\bin\x64\R.exe" CMD check --no-manual fearbase_0.0.1.tar.gz
```

## Progress Notes

- 2026-05-31: Tracking document created before implementation edits.
- 2026-05-31: Preferred implementations and non-ignored call sites renamed to
  snake_case; deprecated public compatibility wrappers added.
- 2026-05-31: Source files reorganized into focused snake_case modules,
  including OpenCPU helpers, validation, data resolution, codebook helpers,
  phase helpers, co-occurrence heatmaps, and trial-count files.
- 2026-05-31: Added compatibility-wrapper tests and architecture guard tests
  for preferred export naming and wrapper isolation.
- 2026-05-31: Regenerated roxygen output, ran local tests with 113 passing
  tests, built the package with Pandoc-enabled vignettes, and confirmed
  `R CMD check --no-manual fearbase_0.0.1.tar.gz` finishes with `Status: OK`.
