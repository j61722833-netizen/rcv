# 02_census_api.R
# Skeleton for connecting precinct-level RCV data to U.S. Census demographic variables.
#
# STATUS: This is a planning skeleton, not runnable code. Each section describes
# the steps needed and the anticipated challenges. Fill in as the pipeline matures.
#
# Census geography note:
#   The Census does not publish data at the election precinct level. The closest
#   unit is the Voting Tabulation District (VTD), which the Census constructs
#   by asking states to map their precincts to Census block boundaries as of the
#   decennial census year. VTD boundaries and precinct boundaries often differ
#   — especially across election cycles — so matching requires care.
#
#   Available Census geographies for this project:
#     VTD (Voting Tabulation District) — from decennial census (SF1 / PL94-171)
#       covers race, Hispanic origin, total population, group quarters
#       NOT available in ACS (ACS does not tabulate at the VTD level)
#     County subdivision (town/municipality) — available in both decennial and ACS
#       useful for Maine, which is municipality-level in our data
#     Block group / tract — available in ACS, useful for city locales if VTD
#       matching is unreliable
#
# Relevant census years for this project:
#   2020 elections (AK, MA, Albany, Bloomington, Boulder, Eureka, Minnetonka):
#     use 2020 decennial census VTDs (PL94-171 redistricting file)
#   2016 election (Maine): use 2010 decennial census VTDs
#
# Key packages:
#   tidycensus  — Census API wrapper with tidy output (install.packages("tidycensus"))
#   tigris      — Census TIGER/Line shapefiles (install.packages("tigris"))
#   sf          — spatial joins for geographic matching (install.packages("sf"))

library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(stringr)

# ============================================================================
# --- STEP 0: API KEY SETUP ---
# Register for a free Census API key at: https://api.census.gov/data/key_signup.html
# Store the key in .Renviron (recommended) or pass it directly.
# ============================================================================

# Option A — store permanently in .Renviron (run once, then restart R):
#   usethis::edit_r_environ()
#   Add line: CENSUS_API_KEY=your_key_here

# Option B — set for the current session only:
#   census_api_key("your_key_here", install = FALSE)

# Verify the key is loaded:
# Sys.getenv("CENSUS_API_KEY")

# ============================================================================
# --- STEP 1: IDENTIFY AVAILABLE CENSUS VARIABLES ---
# Use load_variables() to browse what is available at the VTD level.
# ============================================================================

# Browse the 2020 PL94-171 redistricting file (the one with VTD-level data)
# pl_vars <- load_variables(2020, "pl", cache = TRUE)
# View(pl_vars)

# Key variable groups in PL94-171:
#   P1 — Race (total population by race)
#   P2 — Hispanic or Latino origin by race
#   P3 — Race for population 18+
#   P4 — Hispanic or Latino origin by race for population 18+
#   H1 — Occupancy status (total housing units)

# Key variable groups in ACS 5-year (county subdivision level for Maine):
#   B01001 — Age and sex
#   B02001 — Race
#   B19013 — Median household income
#   B15003 — Educational attainment (population 25+)
#   B05001 — Nativity and citizenship

# Example: total population 18+ (voting age)
# P3_001N = total population 18+
# P3_003N = white alone (18+)
# P3_004N = Black or African American alone (18+)
# P4_002N = Hispanic or Latino (18+)

# ============================================================================
# --- STEP 2: LOAD VTD DATA FOR EACH STATE ---
# get_decennial() can return VTD-level data when geography = "voting district".
# Requires state FIPS code and, for some states, county FIPS code.
# ============================================================================

# State FIPS codes for locales in this project:
fips_codes <- list(
  alaska        = "02",
  massachusetts = "25",
  maine         = "23",   # NOTE: use 2010 census for 2016 election
  california    = "06",   # Albany and Eureka
  minnesota     = "27",   # Bloomington and Minnetonka
  colorado      = "08"    # Boulder
)

# County FIPS for city locales (needed to subset VTD calls):
county_fips <- list(
  albany_county     = "001",  # Alameda County, CA
  boulder_county    = "013",  # Boulder County, CO
  eureka_county     = "023",  # Humboldt County, CA
  bloomington_county = "053", # Hennepin County, MN
  minnetonka_county  = "053"  # Hennepin County, MN (same)
)

# Example: pull 2020 VTD data for Alaska
# ak_vtd <- get_decennial(
#   geography = "voting district",
#   variables = c(
#     total_vap   = "P3_001N",
#     white_vap   = "P3_003N",
#     black_vap   = "P3_004N",
#     hispanic_vap = "P4_002N"
#   ),
#   state = fips_codes$alaska,
#   year  = 2020,
#   sumfile = "pl"    # PL94-171 redistricting file
# )

# Example: pull 2020 VTD data for Hennepin County, MN (Bloomington + Minnetonka)
# mn_hennepin_vtd <- get_decennial(
#   geography = "voting district",
#   variables = c(total_vap = "P3_001N", white_vap = "P3_003N"),
#   state  = fips_codes$minnesota,
#   county = county_fips$bloomington_county,
#   year   = 2020,
#   sumfile = "pl"
# )

# ============================================================================
# --- STEP 3: MATCH PRECINCT IDs TO VTD GEOIDS ---
# This is the hardest step. Matching strategies vary by locale.
# ============================================================================

# --- STRATEGY A: Direct name match (MN locales) ---
# The Minnesota statewide pres file already contains VTDID (from minnesota_pres_2020.csv).
# VTDID is a 9-digit code that can be mapped to Census GEOID.
# Census GEOID for MN VTDs = state(2) + county(3) + vtd(6) = 11 digits total.
# The VTDID in the MN file is 9 digits: county(3) + vtd(6) or similar — verify format.
#
# Once verified, join directly:
# bloomington_census <- bloomington_combine %>%
#   mutate(GEOID = paste0(fips_codes$minnesota, VTDID)) %>%
#   left_join(mn_hennepin_vtd, by = "GEOID")

# --- STRATEGY B: Name/string match (Alaska, Massachusetts) ---
# Match the precinct_id string in our data to the NAME field in the Census VTD file.
# Requires cleaning: strip punctuation, standardize case, abbreviations.
# This will need manual review of unmatched rows.
#
# ak_vtd_geoids <- ak_vtd %>%
#   mutate(vtd_name_clean = str_to_upper(str_squish(NAME)))
# ak_combine_matched <- ak_combine %>%
#   mutate(vtd_name_clean = str_to_upper(str_squish(precinct_id))) %>%
#   left_join(ak_vtd_geoids, by = "vtd_name_clean")

# --- STRATEGY C: Spatial join (city locales without VTD name matches) ---
# 1. Load VTD shapefile for the state/county using tigris::voting_districts()
# 2. Geocode precinct centroids OR obtain a precinct shapefile from the county
# 3. Spatial join (sf::st_join) to assign each precinct to a VTD
# 4. Use the VTD demographic data for the matched VTD
#
# This approach is appropriate when:
#   - Precinct IDs in our data are not in Census VTD records
#   - Boundaries changed between the census year and the election year
#
# vtd_shapes <- voting_districts(state = fips_codes$california,
#                                county = county_fips$albany_county,
#                                year = 2020)
# # Obtain Albany precinct shapefile from Alameda County GIS (external source)
# albany_precincts <- st_read("data/raw/albany_precincts.shp")
# albany_with_vtd  <- st_join(albany_precincts, vtd_shapes, join = st_intersects)

# --- STRATEGY D: Municipality join (Maine) ---
# Maine data is at the municipality level, not true precincts.
# Join to county subdivision (cousub) ACS data, which maps to town/city names.
#
# maine_cousub <- get_acs(
#   geography = "county subdivision",
#   variables = c(median_income = "B19013_001", bach_degree = "B15003_022"),
#   state     = fips_codes$maine,
#   year      = 2020   # or 2016 if using 5-year ACS ending in 2016
# )
# maine_combine_census <- maine_combine %>%
#   mutate(name_clean = str_to_upper(str_squish(precinct_id))) %>%
#   left_join(maine_cousub %>% mutate(name_clean = str_to_upper(NAME)),
#             by = "name_clean")

# ============================================================================
# --- STEP 4: SELECT DEMOGRAPHIC VARIABLES FOR ANALYSIS ---
# Once matched, add the following variables to states_and_cities.
# ============================================================================

# Suggested variables (all at VTD or nearest available geography):
#
# From 2020 PL94-171 (VTD level):
#   pct_white_vap    = white alone non-hispanic VAP / total VAP
#   pct_black_vap    = Black alone VAP / total VAP
#   pct_hispanic_vap = Hispanic or Latino VAP / total VAP
#   total_vap        = total voting-age population
#
# From ACS 5-year (tract or county subdivision level — not available at VTD):
#   median_income    = B19013_001
#   pct_bach_plus    = (B15003_022 + B15003_023 + B15003_024 + B15003_025) / B15003_001
#                      (bachelor's + master's + professional + doctoral / total 25+)
#   pct_renter       = B25003_003 / B25003_001  (renter-occupied / total occupied)

# ============================================================================
# --- STEP 5: QUALITY CHECKS ---
# ============================================================================

# After matching, check:
#   1. Match rate: how many of our precincts matched to a Census VTD?
#      sum(!is.na(states_and_cities$GEOID)) / nrow(states_and_cities)
#
#   2. Population coverage: are unmatched precincts systematically different?
#      (e.g. small rural precincts, split precincts, zero-population VTDs)
#
#   3. VTD population: some Census VTDs have zero population (e.g. industrial zones).
#      Filter or flag these to avoid division-by-zero in share calculations.
#
#   4. Census year mismatch for Maine: 2016 election vs. 2010 or 2020 census.
#      Check whether municipality boundaries changed significantly between 2010-2016.

# ============================================================================
# --- STEP 6: SAVE ENRICHED DATASET ---
# ============================================================================

# After completing the matching and joining steps above, save:
# load("data/rcv_data.RData")   # loads states_and_cities
# states_and_cities_census <- states_and_cities %>%
#   left_join(census_matched, by = "precinct_id")
# save(states_and_cities_census, file = "data/rcv_data_census.RData")
# write.csv(states_and_cities_census, "data/states_and_cities_census.csv",
#           row.names = FALSE)
