# FEARBASE R Package Maintainer Guide

This guide is written for engineers who need to understand, maintain, and extend the `fearbase` R package. It describes the package architecture, data flow, function families, validation strategy, OpenCPU deployment path, and common maintenance workflows.

## 1. Package Purpose

The package provides analysis and visualization helpers for data exported from the FEARBASE database. The public API focuses on:

- participant-level summaries from long-format data;
- study-level summaries from metadata;
- trial-count and phase summaries;
- co-occurrence heatmaps for measures and phases;
- OpenCPU-compatible helpers for loading uploaded CSV files and returning JSON summaries.

Most user-facing functions return `ggplot2` objects, which lets callers print, theme, compose, or save plots outside the package.

## 2. Repository Layout

| Path | Purpose |
|------------------------------------|------------------------------------|
| `R/` | Package implementation. Each file generally owns one function family. |
| `R/mapping.R` | Central identifier mapping and schema normalization logic. |
| `R/combinedHistograms.R` | Heatmap, co-occurrence, and shared validation helpers. |
| `R/viz-defaults.R` | Package palette and `.onLoad()` ggplot theme defaults. |
| `data/` | CSV data files used during local development and fallback resolution. |
| `data-raw/` | Source and rebuild scripts for internal package data. |
| `R/sysdata.rda` | Internal mapping data loaded by package internals. |
| `tests/testthat/` | Regression tests for plotting and validation behavior. |
| `dev_local/` | Docker/OpenCPU local deployment support. |
| `vignettes/` | User-facing package walkthroughs. |
| `docs/` | Maintainer-facing engineering documentation. |

## 3. Core Data Shapes

The package assumes four main FEARBASE tables.

### Long-Format Data

Common argument name: `dl`.

Typical required columns depend on the function, but the central fields are:

- `study_id`: in raw legacy long data this can contain a condition identifier;
- `condition_id`: normalized condition identifier, added by mapping when absent;
- `participant_id`: participant identifier;
- `measure`: long-format variable name, for example `age`, `sex`, or outcome measures;
- `value`: long-format value;
- `phase`: experimental phase abbreviation;
- `stimulus`: stimulus stream;
- `trial`: trial index.

Long data are frequently repeated by participant, measure, phase, stimulus, and trial. Public functions usually call `distinct()` before aggregation so repeated long rows do not inflate counts.

### Metadata

Common argument name: `md`.

Important columns include:

- `id`: raw condition identifier in legacy metadata;
- `condition_id`: normalized condition identifier;
- `study_id`: normalized study identifier;
- `year`: data collection or publication year, depending on the source export;
- `n_subjects`: participant count;
- `us_type` and `cs_type`: stimulus modality columns;
- `instruction_contingency`: instruction category;
- columns beginning with `reinf`: reinforcement-rate fields;
- `physio_scr_*` or legacy `scr_*`: SCR peak-detection window fields.

Metadata are mapped through `.apply_mapping_to_metadata()` before most validation so raw and normalized schemas can both be accepted.

### Codebook

Common argument name: `cb`.

The codebook is the display-label source of truth. Required columns are:

- `attribute`;
- `abbreviation`;
- `name`.

Rows with `attribute == "phase"` are used to label phases. Rows with `attribute == "measure"` are used to label outcome measures.

### Study Design

Common argument name: `sd`.

Important columns are:

- `study_id`;
- `name`: phase abbreviation;
- `cspTrials`;
- `csmTrials`.

Study-design functions use `.apply_mapping_to_study_design()` and the codebook phase labels.

## 4. Identifier Mapping Architecture

The package has one central mapping table with these required columns:

- `condition_id`;
- `study_id`;
- `paper_cond_id`;
- `paper_study_id`.

The mapping source is maintained in `data-raw/mapping.csv`; the runtime object is stored as internal package data in `R/sysdata.rda`.

Rebuild command:

``` r
"C:/Program Files/R/R-4.6.0/bin/x64/Rscript.exe" data-raw/build-mapping.R
```

### Mapping Resolution Order

`.get_mapping()` resolves the mapping in this order:

1.  explicit function argument;
2.  internal `.fearbase_env` cache;
3.  `.GlobalEnv$mapping`, for backwards compatibility;
4.  installed package namespace;
5.  local `R/sysdata.rda`;
6.  error with rebuild guidance.

Mapping identifiers are normalized to character vectors by `.normalize_mapping()` to preserve leading zeros and avoid join mismatches.

### Mapping Application Functions

`mapping.R` provides three schema-specific mapping functions:

- `.apply_mapping_to_long_data(dl, mapping = NULL)`;
- `.apply_mapping_to_metadata(md, mapping = NULL)`;
- `.apply_mapping_to_study_design(sd, mapping = NULL)`.

These helpers are intentionally idempotent: if a data frame already contains a non-missing `condition_id`, the helper returns it unchanged. Maintainers should therefore join optional mapping columns, such as `paper_study_id`, explicitly when a downstream function needs them and the idempotent path may skip them. `trialsPhaseParticipant.R` does this through `.augment_trials_phase_grouping()`.

## 5. Input Resolution Pattern

Several functions allow omitted data arguments for interactive and OpenCPU-like workflows. The normal resolution pattern is:

1.  use the explicit data argument when supplied;
2.  look for a conventional object in the caller environment, such as `data_long`, `metadata`, `codebook`, or `study_design`;
3.  fall back to the package-bundled CSV in `data/`;
4.  stop with a direct package-level error if no source is available.

Key resolver helpers:

- `.resolve_sample_size_long_data()`;
- `.resolve_codebook()`;
- `.resolve_trials_phase_study_design()`.

When adding new functions, prefer explicit data arguments and reuse these resolver helpers only when the function belongs to an existing fallback workflow.

## 6. Public Function Families

### Participant Demographics

| Function | Source | Output | Notes |
|------------------|------------------|------------------|------------------|
| `age()` | long data | `ggplot` | Histogram or ridge density by grouping column. |
| `ageDescriptives()` | long data | tibble | Mean, SD, min, max, and n for age. |
| `sex()` | long data | `ggplot` | Participant-level sex/gender pie chart. |

Important call chain:

`age()` -\> `.apply_mapping_to_long_data()` -\> validate `measure`, `value`, `participant_id`, grouping column -\> filter `measure == "age"` -\> coerce numeric ages -\> plot.

`sex()` -\> `.resolve_sample_size_long_data()` -\> `.apply_mapping_to_long_data()` -\> build distinct participant index -\> normalize reported sex/gender values -\> infer not-reported participants -\> plot.

### Metadata Summaries

| Function | Source | Output | Notes |
|------------------|------------------|------------------|------------------|
| `dataCollectionYear()` | metadata | `ggplot` | Counts records per year. |
| `instructions()` | metadata | `ggplot` | Counts contingency-instruction categories once per study. |
| `stimModality()` | metadata | `ggplot` | Pie chart for `us_type` or `cs_type`. |
| `reinforcementRates()` | metadata | `ggplot` | Distribution of reinforcement-rate entries. |
| `peakDetectionWindows()` | metadata | `ggplot` | SCR baseline, peak, and trough windows. |

Metadata functions all normalize identifiers before selecting columns. Numeric metadata fields are validated before plotting; non-missing malformed values stop the function instead of being silently dropped.

### Trial and Phase Summaries

| Function | Source | Output | Notes |
|------------------|------------------|------------------|------------------|
| `trialsPhaseParticipant()` | long data plus codebook | `ggplot` | Trial-count distribution by phase. |
| `studyDesign()` | study design plus codebook | `ggplot` | Study-design trial-count distribution. Internal, not exported. |

Important call chain:

`trialsPhaseParticipant()` -\> `.validate_trials_phase_y_axis()` -\> `.prepare_trials_phase_participant_data()` -\> long-data resolver -\> codebook resolver -\> mapping -\> grouping augmentation -\> trial counting -\> phase labels from codebook -\> plot branch for participant or study counts.

The long-data branch counts the maximum trial number per participant, phase, and stimulus, then sums stimulus-specific maxima to phase-level participant totals. The function currently excludes `int` and `other` phases.

### Heatmaps

| Function | Source | Output | Notes |
|------------------|------------------|------------------|------------------|
| `measuresHeatmap()` | long data, metadata, codebook | patchwork object | Measure co-occurrence plus marginal counts. |
| `phasesHeatmap()` | long data, codebook | patchwork object | Phase co-occurrence plus marginal counts. |
| `plot_co_occurrence_heatmap()` | precomputed long table | `ggplot` | Reusable heatmap helper. |
| `plot_horizontal_bar()` | precomputed counts | `ggplot` | Reusable marginal bar helper. |

Important call chain:

`measuresHeatmap()` -\> validate inputs -\> map long data and metadata -\> join by `condition_id` -\> label measures from codebook -\> distinct participant-condition measure rows -\> `.get_co_occurrence_data()` -\> reusable plot helpers -\> `arrange_histogram_layout()`.

`phasesHeatmap()` follows the same shape, but labels phases and excludes `int` and `other`.

### Utility and OpenCPU Helpers

| Function | Purpose |
|------------------------------------|------------------------------------|
| `checkData()` | Retrieve an object by name from an environment with strict validation. |
| `createCsv()` | Read an uploaded CSV file; used by OpenCPU upload workflows. |
| `jsonSummary()` | Return a JSON summary of a named object. |
| `updateMapping()` | Load the integrated mapping and optionally assign it globally. |
| `allStudies()` | Return sorted study IDs from mapped metadata. |
| `traceRemovedRows()` | Diagnose rows removed by a ggplot layer. |

`createCsv()` and `jsonSummary()` are currently OpenCPU-oriented and are not exported in `NAMESPACE`. If they are intended to be part of the stable package API, add explicit roxygen `@export` tags and tests before relying on them from external code.

## 7. Validation Strategy

The package validates early and close to function boundaries. Shared helpers in `combinedHistograms.R` include:

- `.validate_data_frame()`;
- `.validate_required_columns()`;
- `.validate_single_column_name()`;
- `.validate_logical_scalar()`.

When adding a new function:

1.  validate scalar arguments before transforming data;
2.  map FEARBASE identifiers before checking mapped schema columns;
3.  report all missing columns together;
4.  reject malformed non-missing numeric values;
5.  stop on empty analysis data after filtering;
6.  return data frames or plot objects without printing.

## 8. Plotting Defaults

`R/viz-defaults.R` defines FEARBASE colors and updates ggplot defaults in `.onLoad()`. This means loading the package changes the default ggplot theme to `theme_classic()` and sets package palettes for discrete and continuous scales.

Maintainers should be careful when adding tests that compare exact plot themes: the package-level `.onLoad()` behavior is part of the runtime contract.

## 9. OpenCPU Local Development

The local OpenCPU deployment lives in `dev_local/`.

Start or rebuild:

``` bash
docker compose up -d --build
```

Stop:

``` bash
docker compose down
```

API explorer:

``` text
http://localhost/ocpu/
```

For CSV-backed functions, upload data first through `createCsv`, then pass the returned OpenCPU session key to functions that need the uploaded object.

## 10. Testing and Quality Gates

Use the project R executable:

``` powershell
& "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe" -e "devtools::test()"
```

Useful additional checks:

``` powershell
& "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe" -e "devtools::document()"
& "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe" -e "devtools::check()"
```

If `devtools` is unavailable, use base tooling:

``` powershell
& "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe" -e "testthat::test_dir('tests/testthat')"
& "C:\Program Files\R\R-4.6.0\bin\x64\R.exe" CMD check .
```

## 11. Extension Checklist

Use this checklist for new user-facing functions.

- [ ] Identify the source data shape: long data, metadata, codebook, or study design.
- [ ] Reuse an existing resolver only when omitted-data behavior is part of the intended workflow.
- [ ] Apply the relevant mapping helper before validating mapped identifier columns.
- [ ] Validate all scalar arguments.
- [ ] Validate all required columns and report missing columns together.
- [ ] Validate numeric coercions and reject malformed non-missing values.
- [ ] De-duplicate repeated long-format rows before aggregation.
- [ ] Return a plot or table without printing.
- [ ] Add CRAN-level roxygen2 documentation for new public functions.
- [ ] Add focused testthat coverage for happy path, malformed input, and empty post-filter data.
- [ ] Add or update a vignette section for user-facing behavior.
- [ ] Rebuild documentation and run tests.

## 12. Current Maintenance Notes

- `createCsv()` and `jsonSummary()` have roxygen blocks but are not exported.
- `sampleSizeDescriptives()`, `reinforcementRateDescriptives()`, `trialsPhaseParticipantDescriptive()`, and `studyDesign()` are not exported. Treat them as internal unless the API is intentionally expanded.
- Several tests exercise zero-argument calls. Preserve caller-object and bundled data fallback behavior unless the team decides to make all data inputs explicit.
- The mapping helpers are intentionally idempotent. If a new function needs optional mapping columns, account for the idempotent early return.