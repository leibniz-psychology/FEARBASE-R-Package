# fearbase

<!-- badges: start -->
<!-- badges: end -->

`fearbase` provides analysis, validation, and visualization helpers for data
exported from the FEARBASE database. The package focuses on common summaries
needed when inspecting fear-conditioning studies: participant demographics,
metadata summaries, trial-count plots, reinforcement rates, and co-occurrence
heatmaps for experimental phases and outcome measures.

The public API follows tidyverse-style snake_case names. Historical camelCase
function names are still available as deprecated compatibility wrappers, so
existing OpenCPU deployments and older scripts can migrate gradually.

## Project Metadata

| Field | Value |
| --- | --- |
| Product owner | `< email >` |
| Developer | `< email >` |
| Development URL | `< xxxxx >` |
| Production URL | `< xxxxx >` |
| Language | R |
| Package name | `fearbase` |
| License | GPL-2 |

## Installation

Install the package from a local checkout with:

```r
install.packages(
  "path/to/FEARBASE-R-Package",
  repos = NULL,
  type = "source"
)
```

During development, load the package directly from the repository:

```r
devtools::load_all()
```

This package currently requires R 4.1 or later. Project maintenance commands
should use the R executable at:

```text
C:\Program Files\R\R-4.6.0\bin\x64
```

## Usage

Load the package in the usual way:

```r
library(fearbase)
```

Most functions work with FEARBASE exports in one of three shapes:

- long-format participant data, commonly passed as `dl` or made available as
  an object named `data_long`;
- study metadata, commonly passed as `md` or made available as an object named
  `metadata`;
- study-design data, commonly passed as `sd` or made available as an object
  named `study_design`.

Several functions also use a FEARBASE codebook, passed as `cb` or made
available as an object named `codebook`. The codebook is used to translate
compact database abbreviations into plot labels.

### Participant summaries

```r
age(data_long, type = "histogram", grouping_variable = "study_id")
age(data_long, type = "ridge", grouping_variable = "condition_id")

age_descriptives(data_long, grouping_variable = c("study_id", "condition_id"))

sex(data_long)
sample_size_by_study(data_long, grouping_variable = "study_id")
```

### Metadata summaries

```r
all_studies(metadata)
data_collection_year(metadata)
stimulus_modality(metadata, type = "us_type", level = "n_studies")
stimulus_modality(metadata, type = "cs_type", level = "n_subjects")
```

### Experimental-design and trial summaries

```r
trial_phase_counts(
  data_long,
  y_axis = "participants",
  grouping_variable = "paper_study_id",
  cb = codebook
)

reinforcement_rates(metadata, grouping_variable = "study_id")
peak_detection_windows(metadata)
peak_detection_windows_dynamic(metadata)
```

### Co-occurrence heatmaps

```r
measures_heatmap(data_long, metadata, codebook)
phases_heatmap(data_long, codebook)
phases_heatmap(data_long, codebook, exclude = c("int", "other"))
```

The lower-level plotting helpers `plot_co_occurrence_heatmap()` and
`plot_horizontal_bar()` are exported for pre-aggregated data when the standard
FEARBASE summaries are not the right entry point.

## Public Function Overview

| Area | Preferred functions | Purpose |
| --- | --- | --- |
| Input and API helpers | `create_csv()`, `json_summary()`, `check_data()`, `instructions()` | Read uploaded CSV data, create JSON summaries, validate exported data, and expose API instructions. |
| Mapping | `update_mapping()` | Load the integrated study-to-condition mapping used to normalize identifiers. |
| Participant summaries | `age()`, `age_descriptives()`, `sex()`, `sample_size_by_study()` | Summarize age, sex or gender, and sample-size distributions. |
| Metadata summaries | `all_studies()`, `data_collection_year()`, `stimulus_modality()` | Summarize study coverage, collection years, and stimulus modality distributions. |
| Study and trial summaries | `trial_phase_counts()`, `reinforcement_rates()`, `peak_detection_windows()`, `peak_detection_windows_dynamic()` | Inspect trial counts, reinforcement rates, and peak-detection windows. |
| Co-occurrence plots | `measures_heatmap()`, `phases_heatmap()`, `plot_co_occurrence_heatmap()`, `plot_horizontal_bar()` | Visualize how measures and experimental phases co-occur across conditions. |
| Diagnostics | `trace_removed_rows()` | Inspect rows removed during data-processing workflows. |

Deprecated compatibility wrappers include `jsonSummary`, `createCsv`,
`measuresHeatmap`, `phasesHeatmap`, `peakDetectionWindows`, `sampleSizeByStudy`,
`stimModality`, and other historical camelCase names. New code should use the
snake_case function names shown above.

## Data Conventions

The package normalizes current and legacy FEARBASE identifier columns through
an internal study-to-condition mapping. Functions that consume long-format data
usually expect columns such as:

- `study_id`
- `condition_id`
- `participant_id`
- `measure`
- `value`
- `phase`
- `stimulus`
- `trial`

Metadata functions usually expect study-level columns such as:

- `id`
- `study_id`
- `condition_id`
- `n_subjects`
- `us_type`
- `cs_type`
- collection-year fields used by the metadata summaries

Study-design helpers expect phase-level design columns such as:

- `study_id`
- `name`
- `cspTrials`
- `csmTrials`

The exact required columns differ by function. See the function documentation
with `?function_name`, for example:

```r
?trial_phase_counts
?measures_heatmap
?stimulus_modality
```

## OpenCPU Development

You can test how changes to `fearbase` affect the OpenCPU deployment by running
a local OpenCPU server from the `dev_local` directory.

### Start the OpenCPU server

```bash
docker compose up -d --build
```

### Stop the OpenCPU server

```bash
docker compose down
```

### Update the OpenCPU server

After making changes to the package, rebuild the local server:

```bash
docker compose up -d --build
```

If changes do not appear in the browser, the browser may be caching an old
container state. Clear the browser cache or open the OpenCPU API Explorer in a
new private or incognito session.

### Use the OpenCPU API Explorer

Open [http://localhost/ocpu/](http://localhost:80/ocpu/) in a browser to access
the OpenCPU API Explorer.

Package functions and included data are available under:

```text
../library/fearbase/
```

See the [OpenCPU API documentation](https://www.opencpu.org/api.html#api-methods)
for details about requests and API Explorer usage.

### Example OpenCPU request

To run `json_summary()` on `randomData`, submit:

- Method: `POST`
- Endpoint: `../library/fearbase/R/json_summary`
- Param name: `d`
- Param value: `"randomData"`

Snake_case endpoints are preferred. Historical camelCase endpoint names such as
`jsonSummary` and `createCsv` remain temporarily available as deprecated
compatibility wrappers.

To inspect the result, pick one of the links returned by OpenCPU and submit:

- Method: `GET`
- Endpoint: `/ocpu/tmp/<hash>/stdout`

### Upload data for OpenCPU requests

Functions that require datasets, such as `age()` or summaries using
`data_long`, need the relevant CSV file to be uploaded first.

1. Send a `POST` request to endpoint `../library/fearbase/R/create_csv`.
2. Click `Add file`.
3. Use `file` as the parameter name, matching the `create_csv()` argument.
4. Browse to the CSV file and send the request.
5. Copy the OpenCPU session key from a returned path such as
   `/ocpu/tmp/x0d715a6f605d54/console`.

The session key is the part after `/ocpu/tmp/`, for example:

```text
x0d715a6f605d54
```

To run `age()` with the uploaded long-data CSV:

- Method: `POST`
- Endpoint: `../library/fearbase/R/age`
- Param name: `dl`
- Param value: `x0d715a6f605d54`

OpenCPU returns links for outputs such as plots. A graphic response can be
opened with a URL similar to:

```text
http://localhost:8004/ocpu/tmp/x0d715a6f605d54/graphics/1
```

## Mapping Maintenance

The study-to-condition mapping is part of the package.

- The editable source file lives at `data-raw/mapping.csv`.
- The runtime object used by the package is the internal dataset `.mapping` in
  `R/sysdata.rda`.
- `update_mapping()` loads and returns the integrated mapping. Set
  `assign_global = TRUE` to also assign it into the calling environment for
  interactive workflows.

After changing `data-raw/mapping.csv`, rebuild the internal mapping with:

```bash
"C:\\Program Files\\R\\R-4.6.0\\bin\\x64\\Rscript.exe" data-raw/build-mapping.R
```

## Development

Common development tasks are:

```r
devtools::document()
devtools::test()
devtools::check()
```

Before committing package changes, run:

```r
devtools::check()
```

The package uses tidyverse conventions for public function names, argument
names, and data manipulation. New exported functions should include roxygen2
documentation and examples that reflect FEARBASE data conventions.

## Useful Links

- [R Packages, 2nd edition](https://r-pkgs.org/whole-game.html)
- [Air R formatter](https://posit-dev.github.io/air/formatter.html)
- [OpenCPU API documentation](https://www.opencpu.org/api.html#api-methods)
