# 02_census_api.R
# Enriches precinct-level RCV data with Census demographics.
#
# Input:  data/rcv_data.RData (states_and_cities, 3446 rows)
# Output: data/rcv_data_census.RData and data/states_and_cities_census.csv
#
# Variables added:
#   PL94-171 (VTD/county subdivision level):
#     total_vap, white_vap, black_vap, hispanic_vap
#     pct_white_vap, pct_black_vap, pct_hispanic_vap
#   ACS 5-year (county subdivision level):
#     median_income, pct_bach_plus, pct_renter

library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(stringr)

options(tigris_use_cache = TRUE)

# --- API key setup ---
stopifnot("Set CENSUS_API_KEY env var before running (e.g. in ~/.Renviron)" =
            nzchar(Sys.getenv("CENSUS_API_KEY")))

# --- Load base data ---
load("data/rcv_data.RData")

# PL94-171 variables for all 2020 locales
pl_vars <- c(total_vap = "P3_001N", white_vap = "P3_003N",
             black_vap = "P3_004N", hispanic_vap = "P4_002N")

# Helper: compute PL share columns
add_pct_cols <- function(df) {
  df %>%
    mutate(
      pct_white_vap    = white_vap / total_vap,
      pct_black_vap    = black_vap / total_vap,
      pct_hispanic_vap = hispanic_vap / total_vap
    )
}

# ============================================================================
# PART 1: PL94-171 RACE/ETHNICITY (VTD level)
# ============================================================================

# ============================================================================
# --- 1. MINNESOTA (Bloomington + Minnetonka) ---
# VTDID in data is 9 digits: state(2) + county(3) + vtd(4).
# Census GEOID is 11 digits: state(2) + county(3) + vtd(6).
# ============================================================================

cat("=== Minnesota (Bloomington + Minnetonka) ===\n")

mn_census <- get_decennial(
  geography = "voting district",
  variables = pl_vars,
  state = "27", county = "053",
  year = 2020, sumfile = "pl", output = "wide"
) %>%
  st_drop_geometry() %>%
  add_pct_cols()

mn_rows <- states_and_cities %>%
  filter(state == "Minnesota") %>%
  mutate(
    census_geoid = paste0(
      substr(as.character(VTDID), 1, 5),
      str_pad(substr(as.character(VTDID), 6, 9), 6, pad = "0")
    )
  )

mn_matched <- mn_rows %>%
  left_join(
    mn_census %>% select(GEOID, total_vap, white_vap, black_vap, hispanic_vap,
                         pct_white_vap, pct_black_vap, pct_hispanic_vap),
    by = c("census_geoid" = "GEOID")
  )

cat("MN match rate:", sum(!is.na(mn_matched$total_vap)), "/", nrow(mn_matched), "\n")

# ============================================================================
# --- 2. MAINE 2016 ---
# Municipality-level → county subdivision from 2010 decennial census.
# 2010 PL94-171 lacks VAP-by-race tables; use total population race vars.
# ============================================================================

cat("\n=== Maine (2010 county subdivision) ===\n")

pl_vars_2010 <- c(total_vap = "P003001", white_vap = "P003003",
                   black_vap = "P003004", hispanic_vap = "P002002")

maine_census <- get_decennial(
  geography = "county subdivision",
  variables = pl_vars_2010,
  state = "23", year = 2010, sumfile = "pl", output = "wide"
) %>%
  st_drop_geometry() %>%
  add_pct_cols() %>%
  mutate(
    town_raw = str_trim(str_extract(NAME, "^[^,]+")),
    town_clean = str_to_upper(str_replace(
      town_raw,
      "\\s+(city|town|plantation|UT|gore|unorganized territory)$",
      ""
    ))
  )

# Clean our precinct names:
#   - "SAINT" → "ST." to match Census abbreviation
#   - Strip TWP/PLT/TOWNSHIP suffixes
#   - For combined names ("A/B/C TWP"), take first name before "/"
maine_rows <- states_and_cities %>%
  filter(locale == "Maine") %>%
  mutate(
    town_base = str_to_upper(str_trim(precinct_id)),
    # SAINT → ST. (Census uses "St." abbreviation)
    town_base = str_replace(town_base, "^SAINT ", "ST. "),
    # For combined unorganized territories, take first name before "/"
    town_first = str_trim(str_extract(town_base, "^[^/]+")),
    # Strip TWP, PLT, PLANTATION, TOWNSHIP suffixes and anything after
    town_clean = str_replace(town_first, "\\s+(TWP|PLT|PLANTATION|TOWNSHIP)(\\s.*)?$", ""),
    # Strip parenthetical suffixes like "(T17 R5)"
    town_clean = str_replace(town_clean, "\\s*\\(.*\\)$", ""),
    town_clean = str_trim(town_clean)
  )

# Deduplicate Census data: sum where multiple entries share a cleaned name
maine_census_dedup <- maine_census %>%
  group_by(town_clean) %>%
  summarise(across(c(total_vap, white_vap, black_vap, hispanic_vap), sum),
            .groups = "drop") %>%
  add_pct_cols()

maine_matched <- maine_rows %>%
  left_join(
    maine_census_dedup %>% select(town_clean, total_vap, white_vap, black_vap, hispanic_vap,
                                   pct_white_vap, pct_black_vap, pct_hispanic_vap),
    by = "town_clean"
  )

cat("Maine match rate:", sum(!is.na(maine_matched$total_vap)), "/", nrow(maine_matched), "\n")

# Show unmatched for diagnostics
maine_still_unmatched <- maine_matched %>% filter(is.na(total_vap))
if (nrow(maine_still_unmatched) > 0) {
  cat("Maine unmatched (", nrow(maine_still_unmatched), "):",
      paste(head(maine_still_unmatched$precinct_id, 10), collapse = ", "), "...\n")
}

# ============================================================================
# --- 3. ALASKA ---
# Census VTD names: "ABBOTT NO. 1 PRECINCT, ANCHORAGE MUNICIPALITY, ALASKA"
# Our names: "01-446 Aurora " → strip district prefix → "AURORA"
# ~40 rows are "DISTRICT N - ABSENTEE/EARLY VOTING/QUESTION" — non-geographic
# aggregates with no Census VTD; these will remain NA.
# ============================================================================

cat("\n=== Alaska ===\n")

ak_census <- get_decennial(
  geography = "voting district",
  variables = pl_vars,
  state = "02", year = 2020, sumfile = "pl", output = "wide"
) %>%
  st_drop_geometry() %>%
  add_pct_cols() %>%
  mutate(
    vtd_clean = str_to_upper(str_squish(str_extract(NAME, "^[^,]+"))),
    vtd_clean = str_replace(vtd_clean, "\\s+PRECINCT(\\s+\\(PART\\))?$", "")
  ) %>%
  # Sum split VTDs (PART) into one row per precinct name
  group_by(vtd_clean) %>%
  summarise(across(c(total_vap, white_vap, black_vap, hispanic_vap), sum),
            GEOID = first(GEOID), .groups = "drop") %>%
  add_pct_cols()

ak_rows <- states_and_cities %>%
  filter(locale == "Alaska") %>%
  mutate(
    pct_name = str_squish(str_replace(precinct_id, "^\\d+-\\d+\\s+", "")),
    vtd_clean = str_to_upper(pct_name)
  )

# Exact match
ak_matched <- ak_rows %>%
  left_join(
    ak_census %>% select(vtd_clean, GEOID, total_vap, white_vap, black_vap,
                         hispanic_vap, pct_white_vap, pct_black_vap, pct_hispanic_vap),
    by = "vtd_clean"
  )

cat("AK exact match rate:", sum(!is.na(ak_matched$total_vap)), "/", nrow(ak_matched), "\n")

# Fuzzy match for remaining (threshold = 3 edits)
ak_unmatched <- ak_matched %>% filter(is.na(total_vap))
if (nrow(ak_unmatched) > 0) {
  ak_census_names <- unique(ak_census$vtd_clean)
  fuzzy_matches <- sapply(unique(ak_unmatched$vtd_clean), function(name) {
    dists <- adist(name, ak_census_names, partial = TRUE)[1, ]
    best <- which.min(dists)
    if (dists[best] <= 3) ak_census_names[best] else NA_character_
  })

  if (any(!is.na(fuzzy_matches))) {
    fuzzy_lookup <- tibble(
      vtd_clean_orig = names(fuzzy_matches[!is.na(fuzzy_matches)]),
      vtd_clean_census = unname(fuzzy_matches[!is.na(fuzzy_matches)])
    )
    ak_fuzzy_join <- fuzzy_lookup %>%
      left_join(ak_census %>% select(vtd_clean, GEOID, total_vap, white_vap, black_vap,
                                      hispanic_vap, pct_white_vap, pct_black_vap, pct_hispanic_vap),
                by = c("vtd_clean_census" = "vtd_clean"))

    for (i in seq_len(nrow(ak_fuzzy_join))) {
      idx <- which(ak_matched$vtd_clean == ak_fuzzy_join$vtd_clean_orig[i] & is.na(ak_matched$total_vap))
      if (length(idx) > 0) {
        ak_matched$total_vap[idx]         <- ak_fuzzy_join$total_vap[i]
        ak_matched$white_vap[idx]         <- ak_fuzzy_join$white_vap[i]
        ak_matched$black_vap[idx]         <- ak_fuzzy_join$black_vap[i]
        ak_matched$hispanic_vap[idx]      <- ak_fuzzy_join$hispanic_vap[i]
        ak_matched$pct_white_vap[idx]     <- ak_fuzzy_join$pct_white_vap[i]
        ak_matched$pct_black_vap[idx]     <- ak_fuzzy_join$pct_black_vap[i]
        ak_matched$pct_hispanic_vap[idx]  <- ak_fuzzy_join$pct_hispanic_vap[i]
        ak_matched$GEOID[idx]             <- ak_fuzzy_join$GEOID[i]
      }
    }
    cat("AK after fuzzy match:", sum(!is.na(ak_matched$total_vap)), "/", nrow(ak_matched), "\n")
  }
}

# Backfill absentee/early/question rows with district-level averages.
# These rows are "District N - Absentee" etc. — votes from people living in
# district N. Assign population-weighted average demographics of all matched
# geographic precincts in that district.
# Extract district number from two formats:
#   Regular: "01-446 Aurora " → district "01"
#   Absentee: "District 1 - Absentee " → district "1"
# Normalize to integer string for matching.
ak_matched <- ak_matched %>%
  mutate(
    dist_raw = ifelse(
      grepl("^District", precinct_id),
      str_extract(precinct_id, "(?<=District\\s)\\d+"),
      str_extract(precinct_id, "^\\d+")
    ),
    dist_num = as.character(as.integer(dist_raw))
  )

ak_district_avg <- ak_matched %>%
  filter(!is.na(total_vap)) %>%
  group_by(dist_num) %>%
  summarise(
    dist_total_vap    = sum(total_vap),
    dist_white_vap    = sum(white_vap),
    dist_black_vap    = sum(black_vap),
    dist_hispanic_vap = sum(hispanic_vap),
    .groups = "drop"
  ) %>%
  mutate(
    dist_pct_white    = dist_white_vap / dist_total_vap,
    dist_pct_black    = dist_black_vap / dist_total_vap,
    dist_pct_hispanic = dist_hispanic_vap / dist_total_vap
  )

ak_still_na <- which(is.na(ak_matched$total_vap))
for (i in ak_still_na) {
  d <- ak_matched$dist_num[i]
  davg <- ak_district_avg %>% filter(dist_num == d)
  if (nrow(davg) == 1) {
    ak_matched$total_vap[i]         <- davg$dist_total_vap
    ak_matched$white_vap[i]         <- davg$dist_white_vap
    ak_matched$black_vap[i]         <- davg$dist_black_vap
    ak_matched$hispanic_vap[i]      <- davg$dist_hispanic_vap
    ak_matched$pct_white_vap[i]     <- davg$dist_pct_white
    ak_matched$pct_black_vap[i]     <- davg$dist_pct_black
    ak_matched$pct_hispanic_vap[i]  <- davg$dist_pct_hispanic
  }
}

cat("AK after district backfill:", sum(!is.na(ak_matched$total_vap)), "/", nrow(ak_matched), "\n")
ak_final_na <- ak_matched %>% filter(is.na(total_vap))
if (nrow(ak_final_na) > 0) {
  cat("AK still unmatched:", nrow(ak_final_na), "—",
      paste(head(ak_final_na$precinct_id, 5), collapse = ", "), "\n")
}

# ============================================================================
# --- 4. MASSACHUSETTS ---
# Census VTD NAME formats:
#   "North Adams City Ward 1 Precinct 1, Berkshire County, Massachusetts"
#   "Barnstable Town Precinct 1, Barnstable County, Massachusetts"
#   "0101, Suffolk County, Massachusetts" (Boston — numeric ward+precinct)
# Our precinct_id: "NORTH ADAMS_1_1", "BARNSTABLE_-_1", "BOSTON_1_1"
#
# Root causes of prior unmatched:
#   1. Abbreviated localities: "N." → "NORTH", "E." → "EAST", etc. (52 rows)
#   2. Letter-suffix precincts: "7A", "4A" — sub-precincts (27 rows)
#   3. Lawrence letter wards: "LAWRENCE_A_1" vs Census "LAWRENCE_-_1" (24 rows)
# ============================================================================

cat("\n=== Massachusetts ===\n")

ma_census <- get_decennial(
  geography = "voting district",
  variables = pl_vars,
  state = "25", year = 2020, sumfile = "pl", output = "wide"
) %>%
  st_drop_geometry() %>%
  add_pct_cols()

ma_census <- ma_census %>%
  mutate(
    vtd_part = str_trim(str_extract(NAME, "^[^,]+")),
    county   = str_trim(str_extract(NAME, "(?<=,\\s)[^,]+(?=\\sCounty)")),
    is_suffolk_numeric = county == "Suffolk" & str_detect(vtd_part, "^\\d{3,4}\\w?$"),
    locality_raw = ifelse(
      is_suffolk_numeric,
      NA_character_,
      str_trim(str_replace(
        str_extract(vtd_part, "^.*?(?=\\s+(?:Ward|Precinct))"),
        "\\s+(City|Town|city|town)$", ""
      ))
    ),
    locality = ifelse(is_suffolk_numeric, "BOSTON", str_to_upper(locality_raw)),
    ward_num = ifelse(
      is_suffolk_numeric,
      as.character(as.integer(substr(vtd_part, 1, 2))),
      str_extract(vtd_part, "(?<=Ward\\s)\\d+")
    ),
    ward_str = ifelse(is.na(ward_num), "-", ward_num),
    pct_str = ifelse(
      is_suffolk_numeric,
      # Handle codes like "0502A": chars 3-4 are precinct, optional letter after
      as.character(as.integer(substr(vtd_part, 3, 4))),
      str_extract(vtd_part, "(?<=Precinct\\s)\\S+")
    ),
    match_key = paste(locality, ward_str, pct_str, sep = "_")
  )

# Expand directional abbreviations in our data
ma_abbrev <- c(
  "^N\\. "  = "NORTH ",
  "^S\\. "  = "SOUTH ",
  "^E\\. "  = "EAST ",
  "^W\\. "  = "WEST "
)

ma_rows <- states_and_cities %>%
  filter(locale == "Massachusetts") %>%
  mutate(
    parts = str_split(precinct_id, "_"),
    ma_locality = str_to_upper(sapply(parts, `[`, 1)),
    ma_ward = sapply(parts, `[`, 2),
    ma_pct = sapply(parts, `[`, 3)
  ) %>%
  select(-parts)

# Expand abbreviations: "N." → "NORTH", etc.
for (pat in names(ma_abbrev)) {
  ma_rows$ma_locality <- str_replace(ma_rows$ma_locality, pat, ma_abbrev[[pat]])
}

# For letter-suffix precincts (e.g. "7A"), try matching to the base number
ma_rows <- ma_rows %>%
  mutate(
    ma_pct_base = str_extract(ma_pct, "^\\d+"),
    match_key = paste(ma_locality, ma_ward, ma_pct, sep = "_"),
    # Fallback key using base precinct number (strips letter suffix)
    match_key_base = paste(ma_locality, ma_ward, ma_pct_base, sep = "_")
  )

# Primary match on exact key
ma_matched <- ma_rows %>%
  left_join(
    ma_census %>% select(match_key, GEOID, total_vap, white_vap, black_vap,
                         hispanic_vap, pct_white_vap, pct_black_vap, pct_hispanic_vap),
    by = "match_key"
  )

n_primary <- sum(!is.na(ma_matched$total_vap))

# Fallback: for unmatched rows with letter-suffix precincts, try base number
ma_still_unmatched <- ma_matched %>% filter(is.na(total_vap) & ma_pct != ma_pct_base)
if (nrow(ma_still_unmatched) > 0) {
  ma_base_lookup <- ma_census %>%
    select(match_key, total_vap, white_vap, black_vap, hispanic_vap,
           pct_white_vap, pct_black_vap, pct_hispanic_vap) %>%
    rename(match_key_base = match_key)

  for (i in seq_len(nrow(ma_still_unmatched))) {
    base_key <- ma_still_unmatched$match_key_base[i]
    census_row <- ma_base_lookup %>% filter(match_key_base == base_key) %>% slice(1)
    if (nrow(census_row) == 1) {
      idx <- which(ma_matched$precinct_id == ma_still_unmatched$precinct_id[i])
      ma_matched$total_vap[idx]         <- census_row$total_vap
      ma_matched$white_vap[idx]         <- census_row$white_vap
      ma_matched$black_vap[idx]         <- census_row$black_vap
      ma_matched$hispanic_vap[idx]      <- census_row$hispanic_vap
      ma_matched$pct_white_vap[idx]     <- census_row$pct_white_vap
      ma_matched$pct_black_vap[idx]     <- census_row$pct_black_vap
      ma_matched$pct_hispanic_vap[idx]  <- census_row$pct_hispanic_vap
    }
  }
}

# Lawrence: letter wards (A-F) → Census has no wards. Use Lawrence place-level data.
ma_lawrence_unmatched <- ma_matched %>%
  filter(is.na(total_vap) & ma_locality == "LAWRENCE")
if (nrow(ma_lawrence_unmatched) > 0) {
  lawrence_place <- get_decennial(
    geography = "place", variables = pl_vars,
    state = "25", year = 2020, sumfile = "pl", output = "wide"
  ) %>%
    st_drop_geometry() %>%
    filter(grepl("Lawrence city", NAME)) %>%
    add_pct_cols()

  if (nrow(lawrence_place) == 1) {
    idx <- which(ma_matched$ma_locality == "LAWRENCE" & is.na(ma_matched$total_vap))
    ma_matched$total_vap[idx]         <- lawrence_place$total_vap
    ma_matched$white_vap[idx]         <- lawrence_place$white_vap
    ma_matched$black_vap[idx]         <- lawrence_place$black_vap
    ma_matched$hispanic_vap[idx]      <- lawrence_place$hispanic_vap
    ma_matched$pct_white_vap[idx]     <- lawrence_place$pct_white_vap
    ma_matched$pct_black_vap[idx]     <- lawrence_place$pct_black_vap
    ma_matched$pct_hispanic_vap[idx]  <- lawrence_place$pct_hispanic_vap
  }
}

cat("MA match rate:", sum(!is.na(ma_matched$total_vap)), "/", nrow(ma_matched),
    "(primary:", n_primary, "+ fallbacks:", sum(!is.na(ma_matched$total_vap)) - n_primary, ")\n")

# ============================================================================
# --- 5. ALBANY, CA ---
# CA counties have "Voting Districts not defined" → use place-level data.
# ============================================================================

cat("\n=== Albany, CA ===\n")

# Reuse CA place data for both Albany and Eureka (single API call)
ca_places <- get_decennial(
  geography = "place", variables = pl_vars,
  state = "06", year = 2020, sumfile = "pl", output = "wide"
) %>%
  st_drop_geometry() %>%
  add_pct_cols()

albany_place <- ca_places %>% filter(grepl("^Albany city", NAME))

albany_matched <- states_and_cities %>%
  filter(locale == "Albany") %>%
  mutate(
    total_vap = albany_place$total_vap[1], white_vap = albany_place$white_vap[1],
    black_vap = albany_place$black_vap[1], hispanic_vap = albany_place$hispanic_vap[1],
    pct_white_vap = albany_place$pct_white_vap[1], pct_black_vap = albany_place$pct_black_vap[1],
    pct_hispanic_vap = albany_place$pct_hispanic_vap[1]
  )

cat("Albany match (city-level):", nrow(albany_matched), "/", nrow(albany_matched), "\n")

# ============================================================================
# --- 6. BOULDER, CO ---
# ============================================================================

cat("\n=== Boulder, CO ===\n")

boulder_census <- get_decennial(
  geography = "voting district", variables = pl_vars,
  state = "08", county = "013", year = 2020, sumfile = "pl", output = "wide"
) %>%
  st_drop_geometry() %>%
  add_pct_cols() %>%
  mutate(vtd_num = str_extract(NAME, "\\d+"))

boulder_matched <- states_and_cities %>%
  filter(locale == "Boulder") %>%
  mutate(vtd_num = substr(precinct_id, nchar(precinct_id) - 2, nchar(precinct_id))) %>%
  left_join(
    boulder_census %>% select(vtd_num, GEOID, total_vap, white_vap, black_vap,
                               hispanic_vap, pct_white_vap, pct_black_vap, pct_hispanic_vap),
    by = "vtd_num"
  )

cat("Boulder match:", sum(!is.na(boulder_matched$total_vap)), "/", nrow(boulder_matched), "\n")

# ============================================================================
# --- 7. EUREKA, CA ---
# ============================================================================

cat("\n=== Eureka, CA ===\n")

eureka_place <- ca_places %>% filter(grepl("^Eureka city", NAME))

eureka_matched <- states_and_cities %>%
  filter(locale == "Eureka") %>%
  mutate(
    total_vap = eureka_place$total_vap[1], white_vap = eureka_place$white_vap[1],
    black_vap = eureka_place$black_vap[1], hispanic_vap = eureka_place$hispanic_vap[1],
    pct_white_vap = eureka_place$pct_white_vap[1], pct_black_vap = eureka_place$pct_black_vap[1],
    pct_hispanic_vap = eureka_place$pct_hispanic_vap[1]
  )

cat("Eureka match (city-level):", nrow(eureka_matched), "/", nrow(eureka_matched), "\n")

# ============================================================================
# PART 2: ACS 5-YEAR DATA (county subdivision / place level)
# Variables: median HH income, % bachelor's+, % renter-occupied
#
# ACS is NOT available at VTD level. Use county subdivision (cousub) for
# statewide locales (maps to MA/ME towns) and place for city locales.
# For AK/MN/CO (VTD-based PL data), use cousub as best available.
# ============================================================================

cat("\n=== ACS 5-year data ===\n")

acs_vars <- c(
  median_income  = "B19013_001",   # median household income
  bach_total     = "B15003_001",   # total pop 25+ (educational attainment denom)
  bach_bachelors = "B15003_022",   # bachelor's degree
  bach_masters   = "B15003_023",   # master's degree
  bach_prof      = "B15003_024",   # professional school degree
  bach_doctorate = "B15003_025",   # doctorate degree
  tenure_total   = "B25003_001",   # total occupied housing units
  tenure_renter  = "B25003_003"    # renter-occupied
)

# --- ACS for statewide locales via county subdivision ---

# Massachusetts (2020 ACS, cousub = towns)
cat("  Fetching MA ACS...\n")
ma_acs <- get_acs(
  geography = "county subdivision",
  variables = acs_vars,
  state = "25", year = 2020, output = "wide"
) %>%
  mutate(
    town_clean = str_to_upper(str_trim(str_replace(
      str_extract(NAME, "^[^,]+"),
      "\\s+(city|town|Town|City)$", ""
    ))),
    pct_bach_plus = (bach_bachelorsE + bach_mastersE + bach_profE + bach_doctorateE) / bach_totalE,
    pct_renter    = tenure_renterE / tenure_totalE
  ) %>%
  select(town_clean, median_income = median_incomeE, pct_bach_plus, pct_renter)

# Maine (2020 ACS for 2016 election — closest available, cousub = towns)
cat("  Fetching ME ACS...\n")
me_acs <- get_acs(
  geography = "county subdivision",
  variables = acs_vars,
  state = "23", year = 2020, output = "wide"
) %>%
  mutate(
    town_clean = str_to_upper(str_trim(str_replace(
      str_extract(NAME, "^[^,]+"),
      "\\s+(city|town|plantation|Town|City|UT|gore|unorganized territory)$", ""
    ))),
    pct_bach_plus = (bach_bachelorsE + bach_mastersE + bach_profE + bach_doctorateE) / bach_totalE,
    pct_renter    = tenure_renterE / tenure_totalE
  ) %>%
  select(town_clean, median_income = median_incomeE, pct_bach_plus, pct_renter)

# Alaska (2020 ACS, cousub — maps to boroughs/census areas)
# AK cousub names won't match precinct names, so use place-level for AK
cat("  Fetching AK ACS (place-level)...\n")
ak_acs_place <- get_acs(
  geography = "place",
  variables = acs_vars,
  state = "02", year = 2020, output = "wide"
) %>%
  mutate(
    place_clean = str_to_upper(str_trim(str_replace(
      str_extract(NAME, "^[^,]+"),
      "\\s+(city|CDP|city and borough|borough|municipality)$", ""
    ))),
    pct_bach_plus = (bach_bachelorsE + bach_mastersE + bach_profE + bach_doctorateE) / bach_totalE,
    pct_renter    = tenure_renterE / tenure_totalE
  ) %>%
  select(place_clean, median_income = median_incomeE, pct_bach_plus, pct_renter)

# --- ACS for city locales via place ---
cat("  Fetching city-level ACS...\n")

# Helper to get place-level ACS for a city
get_city_acs <- function(state_fips, city_pattern) {
  acs <- get_acs(
    geography = "place", variables = acs_vars,
    state = state_fips, year = 2020, output = "wide"
  ) %>%
    filter(grepl(city_pattern, NAME)) %>%
    mutate(
      pct_bach_plus = (bach_bachelorsE + bach_mastersE + bach_profE + bach_doctorateE) / bach_totalE,
      pct_renter = tenure_renterE / tenure_totalE
    )
  list(median_income = acs$median_incomeE[1],
       pct_bach_plus = acs$pct_bach_plus[1],
       pct_renter = acs$pct_renter[1])
}

albany_acs   <- get_city_acs("06", "^Albany city")
boulder_acs  <- get_city_acs("08", "^Boulder city")
eureka_acs   <- get_city_acs("06", "^Eureka city")

# MN cities: Bloomington and Minnetonka as places
mn_acs_places <- get_acs(
  geography = "place", variables = acs_vars,
  state = "27", year = 2020, output = "wide"
) %>%
  mutate(
    place_clean = str_to_upper(str_trim(str_replace(
      str_extract(NAME, "^[^,]+"),
      "\\s+(city)$", ""
    ))),
    pct_bach_plus = (bach_bachelorsE + bach_mastersE + bach_profE + bach_doctorateE) / bach_totalE,
    pct_renter = tenure_renterE / tenure_totalE
  )

bloomington_acs <- mn_acs_places %>% filter(place_clean == "BLOOMINGTON")
minnetonka_acs  <- mn_acs_places %>% filter(place_clean == "MINNETONKA")

cat("  ACS data fetched for all locales.\n")

# ============================================================================
# PART 3: COMBINE PL + ACS AND SAVE
# ============================================================================

cat("\n=== Combining PL94 race data ===\n")

census_cols <- c("total_vap", "white_vap", "black_vap", "hispanic_vap",
                 "pct_white_vap", "pct_black_vap", "pct_hispanic_vap")

all_matched <- bind_rows(
  mn_matched %>% select(precinct_id, locale, all_of(census_cols)),
  maine_matched %>% select(precinct_id, locale, all_of(census_cols)),
  ak_matched %>% select(precinct_id, locale, all_of(census_cols)),
  ma_matched %>% select(precinct_id, locale, all_of(census_cols)),
  albany_matched %>% select(precinct_id, locale, all_of(census_cols)),
  boulder_matched %>% select(precinct_id, locale, all_of(census_cols)),
  eureka_matched %>% select(precinct_id, locale, all_of(census_cols))
) %>%
  group_by(precinct_id, locale) %>%
  summarise(
    across(c(total_vap, white_vap, black_vap, hispanic_vap), ~sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  add_pct_cols() %>%
  mutate(across(everything(), ~ifelse(total_vap == 0, NA_real_, .x)))

# Join PL data
states_and_cities_census <- states_and_cities %>%
  left_join(all_matched, by = c("precinct_id", "locale"))

# --- Join ACS data ---
cat("=== Joining ACS data ===\n")

# Initialize ACS columns
states_and_cities_census <- states_and_cities_census %>%
  mutate(median_income = NA_real_, pct_bach_plus = NA_real_, pct_renter = NA_real_)

# MA: join on town name extracted from precinct_id
ma_idx <- which(states_and_cities_census$locale == "Massachusetts")
ma_towns <- str_to_upper(sapply(str_split(states_and_cities_census$precinct_id[ma_idx], "_"), `[`, 1))
# Expand abbreviations for ACS matching too
for (pat in names(ma_abbrev)) {
  ma_towns <- str_replace(ma_towns, pat, ma_abbrev[[pat]])
}
ma_acs_match <- match(ma_towns, ma_acs$town_clean)
states_and_cities_census$median_income[ma_idx] <- ma_acs$median_income[ma_acs_match]
states_and_cities_census$pct_bach_plus[ma_idx] <- ma_acs$pct_bach_plus[ma_acs_match]
states_and_cities_census$pct_renter[ma_idx]    <- ma_acs$pct_renter[ma_acs_match]

# ME: join on cleaned town name
me_idx <- which(states_and_cities_census$locale == "Maine")
me_towns <- maine_matched$town_clean
me_acs_dedup <- me_acs %>%
  group_by(town_clean) %>%
  summarise(across(everything(), ~first(na.omit(.x))), .groups = "drop")
me_acs_match <- match(me_towns, me_acs_dedup$town_clean)
states_and_cities_census$median_income[me_idx] <- me_acs_dedup$median_income[me_acs_match]
states_and_cities_census$pct_bach_plus[me_idx] <- me_acs_dedup$pct_bach_plus[me_acs_match]
states_and_cities_census$pct_renter[me_idx]    <- me_acs_dedup$pct_renter[me_acs_match]

# AK: join on cleaned precinct name → ACS place name
ak_idx <- which(states_and_cities_census$locale == "Alaska")
ak_pct_names <- str_to_upper(str_squish(str_replace(
  states_and_cities_census$precinct_id[ak_idx], "^\\d+-\\d+\\s+", ""
)))
# Strip trailing "NO. X" for place matching (e.g. "FAIRBANKS NO. 1" → "FAIRBANKS")
ak_place_names <- str_replace(ak_pct_names, "\\s+NO\\.?\\s+\\d+$", "")
ak_acs_match <- match(ak_place_names, ak_acs_place$place_clean)
states_and_cities_census$median_income[ak_idx] <- ak_acs_place$median_income[ak_acs_match]
states_and_cities_census$pct_bach_plus[ak_idx] <- ak_acs_place$pct_bach_plus[ak_acs_match]
states_and_cities_census$pct_renter[ak_idx]    <- ak_acs_place$pct_renter[ak_acs_match]

# AK district-level backfill for ACS: for unmatched precincts, average the ACS
# values of matched precincts in the same legislative district.
ak_df <- states_and_cities_census[ak_idx, ]
ak_df$dist_num <- as.character(as.integer(ifelse(
  grepl("^District", ak_df$precinct_id),
  str_extract(ak_df$precinct_id, "(?<=District\\s)\\d+"),
  str_extract(ak_df$precinct_id, "^\\d+")
)))

ak_dist_acs <- ak_df %>%
  filter(!is.na(median_income)) %>%
  group_by(dist_num) %>%
  summarise(
    dist_median_income = median(median_income, na.rm = TRUE),
    dist_pct_bach_plus = mean(pct_bach_plus, na.rm = TRUE),
    dist_pct_renter    = mean(pct_renter, na.rm = TRUE),
    .groups = "drop"
  )

ak_acs_na <- which(is.na(ak_df$median_income))
for (i in ak_acs_na) {
  d <- ak_df$dist_num[i]
  davg <- ak_dist_acs %>% filter(dist_num == d)
  if (nrow(davg) == 1) {
    states_and_cities_census$median_income[ak_idx[i]] <- davg$dist_median_income
    states_and_cities_census$pct_bach_plus[ak_idx[i]] <- davg$dist_pct_bach_plus
    states_and_cities_census$pct_renter[ak_idx[i]]    <- davg$dist_pct_renter
  }
}

# City locales: assign city-level ACS to all precincts
for (loc_info in list(
  list(loc = "Albany",      acs = albany_acs),
  list(loc = "Boulder",     acs = boulder_acs),
  list(loc = "Eureka",      acs = eureka_acs),
  list(loc = "Bloomington", acs = list(
    median_income = bloomington_acs$median_incomeE[1],
    pct_bach_plus = bloomington_acs$pct_bach_plus[1],
    pct_renter    = bloomington_acs$pct_renter[1])),
  list(loc = "Minnetonka",  acs = list(
    median_income = minnetonka_acs$median_incomeE[1],
    pct_bach_plus = minnetonka_acs$pct_bach_plus[1],
    pct_renter    = minnetonka_acs$pct_renter[1]))
)) {
  idx <- which(states_and_cities_census$locale == loc_info$loc)
  states_and_cities_census$median_income[idx] <- loc_info$acs$median_income
  states_and_cities_census$pct_bach_plus[idx] <- loc_info$acs$pct_bach_plus
  states_and_cities_census$pct_renter[idx]    <- loc_info$acs$pct_renter
}

# ============================================================================
# --- SUMMARY ---
# ============================================================================

cat("\n=== PL94-171 Race/Ethnicity Match Rates ===\n")
states_and_cities_census %>%
  group_by(locale) %>%
  summarise(
    n = n(),
    pl_matched = sum(!is.na(total_vap)),
    pl_pct = round(100 * pl_matched / n, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\n=== ACS Match Rates ===\n")
states_and_cities_census %>%
  group_by(locale) %>%
  summarise(
    n = n(),
    acs_matched = sum(!is.na(median_income)),
    acs_pct = round(100 * acs_matched / n, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nOverall PL match:", sum(!is.na(states_and_cities_census$total_vap)),
    "/", nrow(states_and_cities_census), "\n")
cat("Overall ACS match:", sum(!is.na(states_and_cities_census$median_income)),
    "/", nrow(states_and_cities_census), "\n")

# Save
save(states_and_cities_census, file = "data/rcv_data_census.RData")
write.csv(states_and_cities_census, file = "data/states_and_cities_census.csv",
          row.names = FALSE)
cat("\nSaved: data/rcv_data_census.RData and data/states_and_cities_census.csv\n")
