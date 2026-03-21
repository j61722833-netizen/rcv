# Ranked-Choice Voting Ballot Measure Analysis

Precinct-level analysis of voter support for Ranked-Choice Voting (RCV) ballot measures in the United States. The research question: what factors — especially partisanship, "Low Plurality Winners," and demographics — affect voter support for implementing RCV?

**Author:** Jesse Brandt

## Locales

| Locale          | Year | Level     | RCV Measure          | N precincts |
| --------------- | ---- | --------- | -------------------- | ----------- |
| Alaska          | 2020 | Statewide | Ballot Measure No. 2 | 580         |
| Massachusetts   | 2020 | Statewide | Question 2           | 2,173       |
| Maine           | 2016 | Statewide | Question 5           | 536         |
| Albany, CA      | 2020 | City      | Measure BB           | 3           |
| Bloomington, MN | 2020 | City      | RCV measure          | 32          |
| Boulder, CO     | 2020 | City      | Question 2E          | 88          |
| Eureka, CA      | 2020 | City      | RCV measure          | 11          |
| Minnetonka, MN  | 2020 | City      | RCV measure          | 23          |

## Data Pipeline

The pipeline has four stages, each in `R/`:

### 1. `R/01_clean_and_merge.R` — Clean and merge election data

Reads raw precinct-level election results from `data/raw/`, cleans and merges all 8 locales into a single dataframe (`states_and_cities`). Core output columns:

### 2. `R/02_census_api.R` — Add Census demographics

Enriches precinct data with U.S. Census demographics via the Census API (`tidycensus`). Requires a Census API key (see Setup below).

**PL94-171 race/ethnicity** (VTD-level, 98.5% match rate):

- `pct_white_vap`, `pct_black_vap`, `pct_hispanic_vap` — Voting-age population shares

**ACS 5-year** (county subdivision / place level, 88.2% match rate):

- `median_income` — Median household income (B19013)
- `pct_bach_plus` — % with bachelor's degree or higher (B15003)
- `pct_renter` — % renter-occupied housing units (B25003)

Matching strategies vary by locale: GEOID join (MN), name matching with fuzzy fallback (AK, MA, ME), VTD number extraction (Boulder), and city-level place data (Albany, Eureka). Absentee/early voting aggregates in Alaska are backfilled with district-level averages.

### 3. `R/03_imputation.R` — Multiple imputation of missing demographics

Uses `mice` (predictive mean matching, 5 imputations, 20 iterations) to impute remaining missing Census values. The imputation model **excludes all RCV outcome variables** (`yes_share`, `rcv_yes`, etc.) to prevent leaking the dependent variable into imputed predictors.

### 4. `R/04_models.R` — Fit models on imputed data

Fits GLM/GLMM models on the multiply-imputed data and pools results across all 5 imputations using `mice::pool()`.

## Output Files

| File                          | Description                                              |
| ----------------------------- | -------------------------------------------------------- |
| `data/rcv_data.RData`         | Base election data (3,446 precincts)                     |
| `data/rcv_data_census.RData`  | + Census demographics (observed only)                    |
| `data/rcv_data_imputed.RData` | + Imputed demographics (single completed dataset)        |
| `data/rcv_data_mids.RData`    | `mice` mids object (5 imputations, for pooled inference) |
| `data/model_results.RData`    | Pooled model results from `04_models.R`                  |

## Key Findings

- Higher Democratic vote share correlates with higher RCV support across all locales
- Alaska and Maine form a higher cluster than Massachusetts and the 5 cities
- The "Low Plurality Winner" variable (recent statewide races won with <40%) explains the cluster difference

## Technology

- **R** with tidyverse, tidycensus, tigris, sf, mice
- **Quarto** (.qmd) for reproducible documents
- Raw data in `data/raw/`; Quarto documents and older scripts in `old/`

## Reports

Quarto source files live in `doc/`; rendered PDFs go to `reports/` (viewable directly on GitHub). Render all reports with `quarto render doc/`.

| Report | Description |
| ------ | ----------- |
| [Dem Share vs. RCV Support](reports/dem_vs_yes.html) | Interactive scatterplots (HTML — uses plotly) |
| [Model Results](reports/models.pdf) | Models on original data |
| [Model Results — Imputed](reports/models_imputed.pdf) | Models on multiply-imputed data |
| [Diagnostics](reports/diagnostics.pdf) | Regression diagnostics |
| [Missingness](reports/missingness.pdf) | Missing data patterns |
| [LPW Data Sources](reports/lpw_data_sources.pdf) | Low Plurality Winner data sources |
| [Caveats](reports/caveats.pdf) | Methodological caveats and limitations |

Legacy documents (paper, blog post) are in `old/`.

## Setup

To reproduce the analysis from scratch:

1. Get a free Census API key at <https://api.census.gov/data/key_signup.html>
2. Create a `.env` file in the project root: `CENSUS_API_KEY=your_key_here`
3. Run the pipeline:

```bash
Rscript R/01_clean_and_merge.R
Rscript R/02_census_api.R
Rscript R/03_imputation.R
Rscript R/04_models.R
```
