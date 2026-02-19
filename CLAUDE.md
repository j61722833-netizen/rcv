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
│   └── 01_clean_and_merge.R   # Consolidated data cleaning script
├── data/
│   └── raw/               # Source CSV/txt data files
├── output/                # Rendered documents, plots
├── doc/                   # Quarto documents (future)
└── old/                   # Original flat directory (preserved as-is)
```

## Data Pipeline

### Single cleaning script: `R/01_clean_and_merge.R`

Reads raw data from `data/raw/`, cleans and merges all locales, and produces the `states_and_cities` dataframe.

Locales processed:
- **Alaska 2020** — Ballot Measure No. 2 + U.S. President (`resultsbyprecinct_alaska.txt`)
- **Massachusetts 2020** — Question 2 + U.S. President (`mass_measure_precincts.csv`, `mass_president_precincts.csv`)
- **Maine 2016** — Question 5 + U.S. President (`maine_president_2016.csv`, `maine_referendum16.csv`)
- **Albany CA** — Measure BB + President (`albany_precincts.csv`)
- **Bloomington MN** — RCV measure + President (`bloomington_rcv.csv`, `mn_pres.csv`)
- **Boulder CO** — Question 2E + President (`boulder_rcv_pres.csv`)
- **Eureka CA** — RCV measure + President (`eureka_rcv.csv`, `humboldt_pres.csv`)
- **Minnetonka MN** — RCV measure + President (`minnetonka_rcv.csv`, `mn_pres.csv`)

Run from the project root:
```r
source("R/01_clean_and_merge.R")
```

### Output dataframe: `states_and_cities`
- `dem_share` — Democratic presidential candidate vote share at precinct level
- `yes_share` — RCV ballot measure "Yes" vote share at precinct level
- `place` — Locale name (Alaska, Massachusetts, Maine, Albany, Bloomington, Boulder, Eureka, Minnetonka)

### Quarto documents (in `old/`, to be moved to `doc/` later)
```bash
quarto render old/RCV_Paper.qmd          # Main paper (PDF)
quarto render old/RCV_Blog_Post.qmd      # Blog post (HTML, plotly)
quarto render old/RCV_Limited_Results.qmd # Shortened results (PDF)
```

All Quarto documents load `old/rcv_data.RData` which contains the pre-processed `states_and_cities` dataframe.

### Key Variable Names
- `dem_share` / `biden_share` / `clinton_share` — Democratic presidential candidate vote share at precinct level
- `yes_share` / `rcv_share` — RCV ballot measure "Yes" vote share at precinct level
- `recent_lpw` — Boolean: whether the locale had a recent Low Plurality Winner (Alaska and Maine = TRUE)

## Key Findings (for context when editing)

- Higher Democratic vote share correlates with higher RCV support across all locales
- Alaska and Maine form a higher cluster than Massachusetts and the 5 cities
- The "Low Plurality Winner" variable (recent statewide races won with <40%) explains the cluster difference
- Linear model: `yes_share ~ dem_share + recent_lpw`

## old/ Directory

Contains the entire original project as it was before reorganization. All original R scripts, data files, PDFs, Quarto documents, images, and exploratory code are preserved here unchanged.
