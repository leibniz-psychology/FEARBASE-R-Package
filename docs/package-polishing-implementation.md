# FEARBASE R Package Polishing Implementation

This document tracks the implementation slices for polishing the `fearbase` R package toward stricter R package practice while keeping ignored real-data payloads out of scope.

## Milestones

- [x] Slice 1: Establish the implementation log and confirm non-ignored package scope.
- [x] Slice 2: Centralize cross-cutting utility helpers, now split across
      focused helper files such as `R/validate.R`, `R/resolve-data.R`,
      `R/codebook.R`, `R/phase.R`, and `R/ggplot-diagnostics.R`.
- [x] Slice 3: Replace ignored real-data fallbacks with explicit data or caller-side object resolution.
- [x] Slice 4: Fix mapping validation and tidy-evaluation check issues.
- [x] Slice 5: Add small tracked fixtures and update tests.
- [x] Slice 6: Update vignettes so examples are self-contained.
- [x] Slice 7: Clean package metadata, namespace imports, and build boundaries.
- [x] Slice 8: Review bottlenecks and add lightweight profiling guidance.
- [x] Slice 9: Run test and package-check verification with R 4.6.0.

## Notes

- The repo has no home-level `.gitignore` in this environment; the repository `.gitignore` is the operative ignore source.
- Ignored data payloads such as `data/*.csv`, generated `man/*.Rd`, local Quarto/cache output, and internal binary data are not implementation inputs.
- Real FEARBASE data remains external. Tests and vignettes should use small tracked fixtures instead.
- Local test baseline: `pkgload::load_all("."); testthat::test_dir("tests/testthat")` passes with 66 tests and no warnings.
- Package build/check baseline: `R CMD build .` and `R CMD check --no-manual fearbase_0.0.1.tar.gz` pass with `Status: OK` when `RSTUDIO_PANDOC` points to `C:/Program Files/Pandoc`.
- Full vignette rendering and vignette output rebuilding now pass with Pandoc 3.9.0.2.

## Bottleneck Review

- Ignored CSV fallbacks were removed from package code, so package functions no longer repeatedly parse large local data files when arguments are omitted.
- Mapping helpers now return immediately for already-mapped schemas and validate required source columns before attempting joins, reducing avoidable join work and making unmapped inputs fail early.
- Co-occurrence heatmaps still use matrix multiplication in `.get_co_occurrence_data()`, which is efficient for the expected small phase and measure category sets. Add a sparse-matrix implementation only if profiling shows category counts growing large enough to make the dense category-by-identifier matrix expensive.
- `trace_removed_rows()` remains a diagnostic helper. If it becomes hot in real workflows, the nested reason-building loop is the first candidate for vectorization because it scales by rows times checked aesthetics.
- Suggested profiling command for future optimization slices:

```r
pkgload::load_all(".")
source("tests/testthat/helper-fixtures.R")
Rprof("fearbase-profile.out", interval = 0.01)
phases_heatmap(fixture_long_data(), fixture_codebook())
measures_heatmap(fixture_long_data(), fixture_metadata(), fixture_codebook())
Rprof(NULL)
summaryRprof("fearbase-profile.out")
```
