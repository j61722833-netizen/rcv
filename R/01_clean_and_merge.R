# 01_clean_and_merge.R
# Cleans and merges all RCV ballot measure data at the precinct level.
# Produces: states_and_cities dataframe with columns: dem_share, yes_share, place
#
# Sources:
#   Alaska 2020 — Ballot Measure No. 2 + U.S. President
#   Massachusetts 2020 — Question 2 (RCV) + U.S. President
#   Maine 2016 — Question 5 (RCV) + U.S. President
#   Albany CA, Bloomington MN, Boulder CO, Eureka CA, Minnetonka MN — 2020 city measures + U.S. President

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)

# ============================================================================
# --- ALASKA 2020 ---
# Source: https://www.elections.alaska.gov/results/prior-results/
# ============================================================================

ak_results <- read.csv("data/raw/alaska_all_results_2020.txt", header = FALSE)
names(ak_results) <- c("district", "race", "V3", "V4", "choice", "party", "V7", "votes", "V9")

ak_results <- ak_results %>%
  select(district, race, choice, party, votes)

ak_results <- ak_results %>%
  filter(!choice %in% c("Number of Precincts for Race",
                         "Number of Precincts Reporting", "Registered Voters",
                         "Times Counted"))

ak_pres <- ak_results %>% filter(race == "U.S. President / Vice President")
ak_pres <- ak_pres %>%
  select(district, race, party, votes) %>%
  pivot_wider(names_from = party, values_from = votes)

ak_m2 <- ak_results %>% filter(race == "Ballot Measure No. 2 - 19AKBE")
ak_m2 <- ak_m2 %>%
  pivot_wider(names_from = choice, values_from = votes) %>%
  select(district, race, YES, NO)

ak_merged <- right_join(ak_m2, ak_pres, by = "district") %>%
  select(-race.x, -race.y)

ak_merged <- ak_merged %>%
  mutate(yes_share = YES / (YES + NO),
         biden_share = DEM / (DEM + CON + ALI + GRN + LIB + NOM + REP))

ak_combine <- ak_merged %>%
  # REMOVED: PRECINCT ID (district)
  select(dem_share = biden_share, yes_share) %>%
  mutate(place = "Alaska")

# ============================================================================
# --- MASSACHUSETTS 2020 ---
# Source: https://electionstats.state.ma.us/
# ============================================================================

mass_rcv <- read.csv("data/raw/massachusetts_rcv_2020.csv")
mass_pres <- read.csv("data/raw/massachusetts_pres_2020.csv")

mass_pres <- mass_pres %>%
  filter(!is.na(No.Preference), City.Town != "TOTALS") %>%
  rename(Locality = City.Town)
mass_pres <- mass_pres %>%
  mutate(precinct = paste(Locality, Ward, Pct, sep = "_")) %>%
  rename(biden = Joseph.R..Biden..Jr., total = Total.Votes.Cast)

mass_rcv <- mass_rcv %>%
  filter(Locality != "TOTALS") %>%
  mutate(precinct = paste(Locality, Ward, Pct, sep = "_"))

mass_pres <- mass_pres %>%
  mutate(biden = parse_number(biden), total = parse_number(total)) %>%
  mutate(biden_share = biden / total)

mass_rcv <- mass_rcv %>%
  mutate(Yes = parse_number(Yes), total = parse_number(Total.Votes.Cast)) %>%
  mutate(yes_share = Yes / total)

mass_votes <- inner_join(mass_pres, mass_rcv, by = "precinct")

mass_combine <- mass_votes %>%
  # REMOVED: PRECINCT ID (precinct)
  select(dem_share = biden_share, yes_share) %>%
  mutate(place = "Massachusetts")

# ============================================================================
# --- MAINE 2016 ---
# Source (Pres): MIT Election Data + Science Lab (MEDSL), http://dx.doi.org/10.7910/DVN/LYWX3D
# Source (RCV): Maine Secretary of State, https://www.maine.gov/sos/cec/elec/results/
# ============================================================================

maine_pres <- read.csv("data/raw/maine_pres_2016.csv")
maine_ref <- read.csv("data/raw/maine_rcv_2016.csv")

maine_ref <- maine_ref %>%
  filter(X != "", X != "CTY") %>%
  select(county = X, dist = X.1, rcv_yes = Question.5..Citizen.Initiative,
         rcv_no = X.10, rcv_blank = X.11) %>%
  mutate(across(rcv_yes:rcv_blank, parse_number)) %>%
  mutate(rcv_share = rcv_yes / (rcv_yes + rcv_no + rcv_blank))

maine_pres <- maine_pres %>%
  filter(X != "", X != "CTY") %>%
  select(county = X..., dist = X, clinton = Clinton..Hillary.R., total = TBC) %>%
  mutate(across(clinton:total, parse_number)) %>%
  mutate(clinton_share = clinton / total)

maine_2016 <- full_join(maine_pres, maine_ref, by = "dist")

maine_combine <- maine_2016 %>%
  # REMOVED: PRECINCT ID (dist)
  select(dem_share = clinton_share, yes_share = rcv_share) %>%
  mutate(place = "Maine")

# ============================================================================
# --- ALBANY, CA ---
# Source: https://www.acvote.org/election-information/elections?id=241#
# ============================================================================

albany <- read.csv("data/raw/albany_all_results_2020.csv")

albany <- albany %>% rename(Precinct_name = X...Precinct_name)

albany_measure <- albany %>%
  filter(Contest_title == "Measure BB - City of Albany") %>%
  select(Precinct_name, Contest_title, total_ballots, total_votes, candidate_name) %>%
  pivot_wider(names_from = candidate_name, values_from = total_votes) %>%
  mutate(yes_share = YES / total_ballots)

albany_pres <- albany %>%
  filter(Contest_title == "President and Vice President") %>%
  select(Precinct_name, Contest_title, total_ballots, total_votes, candidate_name) %>%
  pivot_wider(names_from = candidate_name, values_from = total_votes) %>%
  select(Precinct_name, Contest_title, total_ballots,
         biden = "JOSEPH R. BIDEN AND KAMALA D. HARRIS",
         trump = "DONALD J. TRUMP AND MICHAEL R. PENCE") %>%
  mutate(biden_share = biden / total_ballots)

albany_both <- left_join(albany_measure, albany_pres, by = "Precinct_name") %>%
  # REMOVED: PRECINCT ID (Precinct_name) — kept temporarily for join, dropped below
  select(biden_share, yes_share) %>%
  mutate(city = "Albany")

# ============================================================================
# --- BLOOMINGTON, MN ---
# Source (RCV): https://electionresults.sos.state.mn.us/Results/Index?ersElectionId=136&scenario=ResultsByPrecinctCrosstab&QuestionId=1202
# Source (Pres): https://www.sos.state.mn.us/elections-voting/election-results/2020/2020-general-election-results/2020-precinct-results-spreadsheet/
# ============================================================================

bloomington_measure <- read.csv("data/raw/bloomington_rcv_2020.csv") %>%
  rename(precinct = X...County..Precinct) %>%
  filter(precinct != "Candidate Totals:")
bloomington_measure <- bloomington_measure %>%
  mutate(precinct = gsub("Hennepin: ", "", precinct))

mn_pres <- read.csv("data/raw/minnesota_pres_2020.csv") %>%
  rename(precinct = PCTNAME, VTDID = X...VTDID)

bloomington_both <- left_join(bloomington_measure, mn_pres, by = "precinct") %>%
  select(precinct, YES, NO, USPRSDFL, USPRSTOTAL, TOTVOTING)
bloomington_both <- bloomington_both %>%
  mutate(across(c(2:3), parse_number)) %>%
  mutate(yes_share = YES / TOTVOTING, biden_share = USPRSDFL / TOTVOTING) %>%
  # REMOVED: PRECINCT ID (precinct)
  select(biden_share, yes_share) %>%
  mutate(city = "Bloomington")

# ============================================================================
# --- BOULDER, CO ---
# Source: https://assets.bouldercounty.org/wp-content/uploads/2020/11/2020-Boulder-County-General-Election-Official-Statement-of-Votes.xlsx
# ============================================================================

boulder <- read.csv("data/raw/boulder_all_results_2020.csv") %>%
  rename(Precinct.Name.Short = X...Precinct.Name..Short.)

boulder_rcv <- boulder %>%
  filter(Contest.Name == "City of Boulder Ballot Question 2E") %>%
  select(precinct = Precinct.Name, Contest.Name, Choice.Name, Total.Votes, Total.Ballots) %>%
  pivot_wider(names_from = Choice.Name, values_from = Total.Votes)
boulder_rcv <- boulder_rcv %>%
  rename(yes = "YES/FOR") %>%
  mutate(yes_share = parse_number(yes) / parse_number(Total.Ballots))

boulder_pres <- boulder %>%
  filter(Contest.Name == "Presidential Electors") %>%
  select(precinct = Precinct.Name, Contest.Name, Choice.Name, Total.Votes, Total.Ballots) %>%
  pivot_wider(names_from = Choice.Name, values_from = Total.Votes)
boulder_pres <- boulder_pres %>%
  select(precinct, biden = "Joseph R. Biden / Kamala D. Harris", Total.Ballots) %>%
  mutate(biden_share = parse_number(biden) / parse_number(Total.Ballots))

boulder_both <- left_join(boulder_rcv, boulder_pres, by = "precinct") %>%
  # REMOVED: PRECINCT ID (precinct)
  select(biden_share, yes_share) %>%
  mutate(city = "Boulder")

# ============================================================================
# --- EUREKA, CA ---
# Source (RCV): https://humboldtgov.org/DocumentCenter/View/91079/FINAL-CANVASS-G2020-PDF
# Source (Pres): https://statewidedatabase.org/d10/g20.html
# ============================================================================

eureka_measure <- read.csv("data/raw/eureka_rcv_2020.csv") %>%
  rename(Precinct = X...Precinct) %>%
  filter(Precinct != "Totals") %>%
  select(precinct = Precinct, Yes, No, Total.Ballots.Cast) %>%
  mutate(across(c(2:4), parse_number)) %>%
  mutate(yes_share = Yes / Total.Ballots.Cast)

eureka_pres <- read.csv("data/raw/humboldt_pres_2020.csv")
eureka_pres <- eureka_pres %>%
  select(svprec, total_votes = TOTVOTE, biden = PRSDEM01, trump = PRSREP01) %>%
  mutate(precinct = gsub("_A", "", svprec)) %>%
  group_by(precinct) %>%
  summarize(across(c(2:4), sum))

eureka_both <- left_join(eureka_measure, eureka_pres, by = "precinct") %>%
  filter(yes_share != is.nan(yes_share)) %>%
  mutate(biden_share = biden / total_votes) %>%
  # REMOVED: PRECINCT ID (precinct)
  select(biden_share, yes_share) %>%
  mutate(city = "Eureka")

# ============================================================================
# --- MINNETONKA, MN ---
# Source (RCV): https://electionresults.sos.state.mn.us/Results/Index?ersElectionId=136&scenario=ResultsByPrecinctCrosstab&QuestionId=1233
# Source (Pres): https://www.sos.state.mn.us/elections-voting/election-results/2020/2020-general-election-results/2020-precinct-results-spreadsheet/
# ============================================================================

minnetonka_measure <- read.csv("data/raw/minnetonka_rcv_2020.csv") %>%
  rename(precinct = X...County..Precinct) %>%
  filter(precinct != "Candidate Totals:") %>%
  mutate(precinct = gsub("Hennepin: ", "", precinct))

minnetonka_both <- left_join(minnetonka_measure, mn_pres, by = "precinct") %>%
  select(precinct, YES, NO, USPRSDFL, USPRSTOTAL, TOTVOTING) %>%
  mutate(across(c(2:3), parse_number)) %>%
  mutate(yes_share = YES / TOTVOTING, biden_share = USPRSDFL / TOTVOTING) %>%
  # REMOVED: PRECINCT ID (precinct)
  select(biden_share, yes_share) %>%
  mutate(city = "Minnetonka")

# ============================================================================
# --- COMBINE ALL ---
# ============================================================================

# Combine the 5 cities
cities_all <- list(albany_both, bloomington_both, boulder_both, eureka_both, minnetonka_both) %>%
  map(select, "biden_share", "yes_share", "city") %>%
  bind_rows()

cities_all2 <- cities_all %>%
  # REMOVED: CITY-SPECIFIC COLUMN (city renamed to place)
  select(dem_share = biden_share, yes_share, place = city)

# Combine the 3 states
states_all <- bind_rows(ak_combine, mass_combine, maine_combine)

# Final combined dataframe
states_and_cities <- bind_rows(states_all, cities_all2)

cat("states_and_cities created:", nrow(states_and_cities), "rows\n")
cat("Columns:", paste(names(states_and_cities), collapse = ", "), "\n")
cat("Places:", paste(unique(states_and_cities$place), collapse = ", "), "\n")

# Save clean dataset
save(states_and_cities, file = "data/rcv_data.RData")
write.csv(states_and_cities, file = "data/states_and_cities.csv", row.names = FALSE)
cat("Saved: data/rcv_data.RData and data/states_and_cities.csv\n")
