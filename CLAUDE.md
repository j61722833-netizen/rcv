# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a political science research project analyzing **Ranked-Choice Voting (RCV) ballot measures** at the precinct level in the United States. The research question: what factors (especially partisanship and "Low Plurality Winners") affect voter support for implementing RCV? Author: Jesse Brandt.

## Technology Stack

- **R** with tidyverse for data wrangling and analysis
- **Quarto (.qmd)** for reproducible documents (paper, blog post, analysis)
- **RStudio** project (`rcv.Rproj`)
- Key R packages: `tidyverse`, `ggplot2`, `stargazer`, `kableExtra`, `plotly`, `ggiraph`, `rvest`, `scales`

## Directory Structure

```
rcv/
├── rcv.Rproj              # RStudio project file
├── CLAUDE.md              # This file
├── R/
│   ├── 01_clean_and_merge.R   # Clean raw data, merge locales → states_and_cities
│   ├── 02_census_api.R        # Enrich with Census demographics (PL94-171 + ACS)
│   ├── 03_imputation.R        # Multiple imputation of missing demographics (mice)
│   ├── 04_models.R            # GLM/GLMM models on multiply-imputed data
│   └── 05_cv.R                # Leave-one-locale-out out-of-sample CV of the models
├── data/
│   ├── raw/               # Source CSV/txt data files
│   └── *.RData, *.csv     # Intermediate and final processed datasets
├── doc/                   # Quarto reports (source .qmd + rendered HTML)
│   ├── dem_vs_yes.qmd     # Dem share vs. RCV support scatterplots
│   ├── models.qmd         # Model results (original data)
│   ├── models_imputed.qmd # Model results (imputed data)
│   ├── diagnostics.qmd    # Model diagnostics
│   ├── missingness.qmd    # Missing data patterns
│   ├── lpw_data_sources.qmd  # Low Plurality Winner data sources
│   └── caveats.qmd        # Study caveats and limitations
├── output/                # (empty, intended for separated rendered output)
└── old/                   # Original flat directory (preserved as-is)
```

## Data Pipeline

Run scripts sequentially from the project root:

```r
source("R/01_clean_and_merge.R")  # → data/rcv_data.RData, data/states_and_cities.csv
source("R/02_census_api.R")       # → data/rcv_data_census.RData (needs CENSUS_API_KEY)
source("R/03_imputation.R")       # → data/rcv_data_imputed.RData, data/rcv_data_mids.RData
source("R/04_models.R")           # → data/model_results.RData
source("R/05_cv.R")               # → data/cv_results.RData, data/cv_metrics.csv
```

### 01: Clean and merge (`R/01_clean_and_merge.R`)

Reads raw data from `data/raw/`, cleans and merges all locales into the `states_and_cities` dataframe.

Locales processed:
- **Alaska 2020** — Ballot Measure No. 2 + U.S. President
- **Massachusetts 2020** — Question 2 + U.S. President
- **Maine 2016** — Question 5 + U.S. President
- **Albany CA, Bloomington MN, Boulder CO, Eureka CA, Minnetonka MN** — local RCV measures + President

### 02: Census enrichment (`R/02_census_api.R`)

Pulls PL94-171 redistricting data and ACS 5-year estimates via Census API. Joins demographics to precincts using geographic crosswalks.

### 03: Imputation (`R/03_imputation.R`)

Multiple imputation of missing Census demographics using `mice`. Produces both a single imputed dataset and the full `mids` object for pooled analysis.

### 04: Models (`R/04_models.R`)

Fits GLM/GLMM models on multiply-imputed data. Pools results across 5 imputations using `mice::pool()`.

### 05: Cross-validation (`R/05_cv.R`)

Leave-one-locale-out out-of-sample evaluation of the models. Each locale is held out and predicted from the rest, respecting the precinct-within-locale clustering. M1 (baseline) needs no demographics so its CV is imputation-free; M2/M3 refit the `mice` imputation inside each training fold (`ignore=`) to avoid leakage. Reports out-of-sample deviance-explained and MAE, pooled across imputations. **Jesse's rule: report CV numbers, not in-sample fit.**

### Key output variables
- `dem_share` — Democratic presidential vote share at precinct level
- `yes_share` — RCV ballot measure "Yes" vote share at precinct level
- `place` — Locale name (Alaska, Massachusetts, Maine, Albany, Bloomington, Boulder, Eureka, Minnetonka)
- `recent_lpw` — Whether the locale had a recent Low Plurality Winner

## Quarto Reports

Reports live in `doc/` and render to HTML in the same directory:

```bash
quarto render doc/dem_vs_yes.qmd       # Dem share vs. RCV support plots
quarto render doc/models.qmd           # Model results (original data)
quarto render doc/models_imputed.qmd   # Model results (imputed data)
quarto render doc/cv.qmd               # Out-of-sample cross-validation
quarto render doc/diagnostics.qmd      # Model diagnostics
quarto render doc/missingness.qmd      # Missing data patterns
quarto render doc/lpw_data_sources.qmd # LPW data sources
quarto render doc/caveats.qmd          # Study caveats
```

Legacy Quarto documents (paper, blog post) remain in `old/` and load `old/rcv_data.RData`.

## Key Findings (for context when editing)

- Higher Democratic vote share correlates with higher RCV support across all locales
- Alaska and Maine form a higher cluster than Massachusetts and the 5 cities
- The "Low Plurality Winner" variable (recent statewide races won with <40%) explains the cluster difference
- Linear model: `yes_share ~ dem_share + recent_lpw`

## Environment Variables

Census API key is stored in `/workspace/.env` (gitignored). To load it in R:

```r
Sys.setenv(CENSUS_API_KEY = readLines("/workspace/.env") |>
  grep("^CENSUS_API_KEY=", value = TRUE) |>
  sub("CENSUS_API_KEY=", "", x = _))
```

Or export before launching R:

```bash
export $(cat /workspace/.env | xargs)
```

Then access via `Sys.getenv("CENSUS_API_KEY")`.

## old/ Directory

Contains the entire original project as it was before reorganization. All original R scripts, data files, PDFs, Quarto documents, images, and exploratory code are preserved here unchanged.
