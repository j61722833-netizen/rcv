# 01_clean_and_merge.R
# Cleans and merges all RCV ballot measure data at the precinct level.
#
# Output: states_and_cities dataframe saved to data/rcv_data.RData and
#         data/states_and_cities.csv
#
# Core output columns (present for every precinct):
#   precinct_id      — unique precinct identifier within the locale
#   locale           — name of the RCV ballot measure geography (e.g. "Alaska", "Albany")
#   state            — U.S. state the precinct belongs to
#   city             — city name for municipal measures; NA for statewide measures
#   rcv_jurisdiction — "statewide" or "local"
#   dem_share        — Democratic presidential candidate vote share (0–1)
#   yes_share        — RCV ballot measure "Yes" vote share (0–1)
#   recent_lpw       — TRUE if the state had a Low Plurality Winner within 10 years
#
# Additional locale-specific columns are preserved; they will be NA for rows
# belonging to other locales.
#
# Sources:
#   Alaska 2020     — Ballot Measure No. 2 + U.S. President
#   Massachusetts 2020 — Question 2 (RCV) + U.S. President
#   Maine 2016      — Question 5 (RCV) + U.S. President
#   Albany CA, Bloomington MN, Boulder CO, Eureka CA, Minnetonka MN
#                   — 2020 local RCV measures + U.S. President

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)

# ============================================================================
# --- ALASKA 2020 ---
# Source: https://www.elections.alaska.gov/results/prior-results/
# File: alaska_all_results_2020.txt
#   No header row; 9 columns: district, race, V3, V4, choice, party, V7, votes, V9
#   V3, V4, V7, V9 are administrative formatting fields that contain only "NP",
#   "Total", or blank — no analytical content. They are documented here but not
#   carried into the analysis.
# ============================================================================

ak_results <- read.csv("data/raw/alaska_all_results_2020.txt", header = FALSE)
names(ak_results) <- c("district", "race", "V3", "V4", "choice", "party", "V7", "votes", "V9")

# Remove aggregate summary rows that are not precinct-level vote counts
ak_results <- ak_results %>%
  filter(!choice %in% c("Number of Precincts for Race",
                         "Number of Precincts Reporting", "Registered Voters",
                         "Times Counted"))

# Keep only the two races relevant to this study: presidential and the RCV measure.
# The file contains results for all 2020 Alaska races; others are out of scope.
ak_pres <- ak_results %>%
  filter(race == "U.S. President / Vice President") %>%
  # NOTE: V3, V4, V7, V9 dropped here — constant admin fields (NP/Total/blank).
  # choice (candidate name) is redundant after pivoting on party code.
  select(district, race, party, votes) %>%
  pivot_wider(names_from = party, values_from = votes) %>%
  rename(
    ak_pres_race = race,
    ak_pres_dem  = DEM,
    ak_pres_rep  = REP,
    ak_pres_lib  = LIB,
    ak_pres_grn  = GRN,
    ak_pres_ali  = ALI,
    ak_pres_nom  = NOM,
    ak_pres_con  = CON
  )

ak_m2 <- ak_results %>%
  filter(race == "Ballot Measure No. 2 - 19AKBE") %>%
  # NOTE: V3, V4, V7, V9 dropped here — constant admin fields (see above).
  # party is NA for ballot measures (no party affiliation).
  select(district, race, choice, votes) %>%
  pivot_wider(names_from = choice, values_from = votes) %>%
  rename(ak_rcv_race = race, ak_rcv_yes = YES, ak_rcv_no = NO)

ak_combine <- right_join(ak_m2, ak_pres, by = "district") %>%
  mutate(
    yes_share        = ak_rcv_yes / (ak_rcv_yes + ak_rcv_no),
    dem_share        = ak_pres_dem / (ak_pres_dem + ak_pres_con + ak_pres_ali +
                                      ak_pres_grn + ak_pres_lib + ak_pres_nom + ak_pres_rep),
    precinct_id      = as.character(district),
    locale           = "Alaska",
    state            = "Alaska",
    city             = NA_character_,
    rcv_jurisdiction = "statewide"
  )

# ============================================================================
# --- MASSACHUSETTS 2020 ---
# Source: https://electionstats.state.ma.us/
# Files: massachusetts_rcv_2020.csv, massachusetts_pres_2020.csv
#   Columns — RCV file: Locality, Ward, Pct, Yes, No, Blanks, Total Votes Cast
#   Columns — Pres file: City.Town, Ward, Pct, Biden, Trump, Jorgensen,
#             Hawkins, All Others, No Preference, Blanks, Total Votes Cast
# ============================================================================

mass_rcv  <- read.csv("data/raw/massachusetts_rcv_2020.csv")
mass_pres <- read.csv("data/raw/massachusetts_pres_2020.csv")

# Remove statewide totals row; construct a precinct identifier from locality + ward + precinct
mass_pres <- mass_pres %>%
  filter(!is.na(No.Preference), City.Town != "TOTALS") %>%
  rename(Locality = City.Town) %>%
  mutate(precinct = paste(Locality, Ward, Pct, sep = "_")) %>%
  rename(
    pres_biden  = Joseph.R..Biden..Jr.,
    pres_trump  = Donald.J..Trump,
    pres_jorg   = Jo.Jorgensen,
    pres_hawk   = Howard.Hawkins,
    pres_other  = All.Others,
    pres_nopref = No.Preference,
    pres_blank  = Blanks,
    pres_total  = Total.Votes.Cast
  ) %>%
  mutate(
    # Larger precincts have comma-formatted vote counts (e.g. "1,234"); smaller
    # candidates (No Preference, All Others) are already numeric since their counts
    # are always small. Use gsub + as.numeric to handle both cases safely.
    across(c(pres_biden, pres_trump, pres_jorg, pres_hawk,
             pres_other, pres_nopref, pres_blank, pres_total),
           ~as.numeric(gsub(",", "", as.character(.x)))),
    biden_share = pres_biden / pres_total
  )

mass_rcv <- mass_rcv %>%
  filter(Locality != "TOTALS") %>%
  mutate(precinct = paste(Locality, Ward, Pct, sep = "_")) %>%
  rename(
    rcv_yes   = Yes,
    rcv_no    = No,
    rcv_blank = Blanks,
    rcv_total = Total.Votes.Cast
  ) %>%
  mutate(
    # All vote-count columns may be comma-formatted in larger precincts;
    # use gsub + as.numeric to handle both character and already-numeric inputs.
    across(c(rcv_yes, rcv_no, rcv_blank, rcv_total),
           ~as.numeric(gsub(",", "", as.character(.x)))),
    # Use contested votes (YES + NO) as denominator for consistency with all
    # other locales. rcv_total (Total.Votes.Cast) includes blanks which would
    # deflate yes_share relative to locales that exclude blanks.
    yes_share = rcv_yes / (rcv_yes + rcv_no)
  )

# Join pres and RCV on the constructed precinct string.
# Shared columns Locality, Ward, Pct appear in both; inner_join keeps .x/.y pairs.
# The .x versions (from pres) and .y versions (from rcv) should be identical
# for matched rows — both are kept so the source of each can be verified.
mass_combine <- inner_join(mass_pres, mass_rcv, by = "precinct") %>%
  mutate(
    precinct_id      = precinct,
    dem_share        = biden_share,
    locale           = "Massachusetts",
    state            = "Massachusetts",
    city             = NA_character_,
    rcv_jurisdiction = "statewide"
  )

# ============================================================================
# --- MAINE 2016 ---
# Source (Pres): MIT Election Data + Science Lab, http://dx.doi.org/10.7910/DVN/LYWX3D
# Source (RCV):  Maine Secretary of State, https://www.maine.gov/sos/cec/elec/results/
# Files: maine_pres_2016.csv, maine_rcv_2016.csv
#
# Both files use a two-row header scheme. R reads row 1 as column names and
# row 2 as the first data row. Blank column names in row 1 receive auto-generated
# names (X, X.1, X.2, …) in reading order.
#
# VERIFY COLUMN NAMES: After loading, run names(maine_ref) and names(maine_pres)
# to confirm the auto-generated names match the mappings below. The mappings for
# Q1–Q4 and Q6 are inferred from the pattern established by the original code's
# Q5 references (rcv_no = X.10, rcv_blank = X.11); they should be checked.
#
# maine_rcv_2016.csv inferred column layout:
#   X     = county code (e.g. AND)
#   X.1   = municipality name (join key)
#   Question.1..Citizen.Initiative, X.2, X.3  = Q1 YES / NO / BLANK
#   Question.2..Citizen.Initiative, X.4, X.5  = Q2 YES / NO / BLANK
#   Question.3..Citizen.Initiative, X.6, X.7  = Q3 YES / NO / BLANK
#   Question.4..Citizen.Initiative, X.8, X.9  = Q4 YES / NO / BLANK
#   Question.5..Citizen.Initiative, X.10, X.11 = Q5 YES / NO / BLANK  [RCV measure]
#   Question.6...Bond.Issue,        X.12, X.13 = Q6 YES / NO / BLANK
#   Total.Ballots.Cast
#
# maine_pres_2016.csv inferred column layout (first column may have a BOM marker,
# causing R to name it X... instead of X):
#   X...  = county code
#   X     = municipality name (join key)
#   Clinton..Hillary.R., Johnson..Gary, Stein..Jill, Trump..Donald.J., …, TBC
# ============================================================================

maine_pres <- read.csv("data/raw/maine_pres_2016.csv")
maine_ref  <- read.csv("data/raw/maine_rcv_2016.csv")

# Guard: verify the auto-generated column names match expectation. Both files
# have a UTF-8 BOM that shifts names (col 1 becomes "X..." instead of "X").
# If the BOM is absent or the file layout changes, column mappings silently land
# on wrong data. These assertions convert that failure mode into a clear error.
stopifnot(
  "maine_pres col 1 must be 'X...' (BOM expected)" = names(maine_pres)[1] == "X...",
  "maine_pres col 2 must be 'X' (municipality name)" = names(maine_pres)[2] == "X",
  "maine_ref col 1 must be 'X...' (BOM expected)"  = names(maine_ref)[1]  == "X...",
  "maine_ref col 3 must be 'X.1' (municipality name)" = names(maine_ref)[3] == "X.1",
  "maine_ref Q5 YES must be at col 16" = names(maine_ref)[16] == "Question.5..Citizen.Initiative"
)

# Remove blank rows (county separator rows) and the second-row label row
# (X... is the county code column; filter on X... not X, because X is an
# asterisk marker column that is blank for most municipalities — filtering
# X != "" would accidentally drop the majority of the data)
maine_ref <- maine_ref %>%
  filter(X... != "", X... != "CTY") %>%
  select(
    county_code  = X...,   # county code (AND, ARO, etc.)
    asterisk_ref = X,      # asterisk marker ("*" or blank); kept for reference
    dist         = X.1,    # municipality name (join key)
    # All six 2016 citizen initiative questions retained.
    # Column names X.2–X.13 are inferred from the Q5 pattern; verify with names().
    q1_yes = Question.1..Citizen.Initiative, q1_no = X.2,  q1_blank = X.3,
    q2_yes = Question.2..Citizen.Initiative, q2_no = X.4,  q2_blank = X.5,
    q3_yes = Question.3..Citizen.Initiative, q3_no = X.6,  q3_blank = X.7,
    q4_yes = Question.4..Citizen.Initiative, q4_no = X.8,  q4_blank = X.9,
    # Question 5 is the RCV citizen initiative
    rcv_yes = Question.5..Citizen.Initiative, rcv_no = X.10, rcv_blank = X.11,
    # Question 6 is a bond issue, retained for completeness
    q6_yes = Question.6...Bond.Issue,         q6_no = X.12,  q6_blank = X.13,
    maine_total_ballots = Total.Ballots.Cast
  ) %>%
  filter(dist != "MUNICIPALITY") %>%   # removes the second header row
  mutate(across(q1_yes:maine_total_ballots, parse_number)) %>%
  # Denominator is YES + NO only (not blank). Blank ballots represent abstention
  # on this question, not opposition. Excluding blanks keeps yes_share comparable
  # with all other locales which use only contested votes in the denominator.
  mutate(rcv_share = rcv_yes / (rcv_yes + rcv_no))

# Remove blank separator rows (X... = "") and the second-row label row
# (X... = "CTY"). Both files use X... as the county code column; filtering on
# X... consistently avoids accidentally dropping real data rows.
# County subtotal rows ("Total:") also have blank X... and are removed here.
maine_pres <- maine_pres %>%
  filter(X... != "", X... != "CTY") %>%
  select(
    county_code      = X...,   # county code (AND, ARO, etc.)
    dist             = X,      # municipality name (join key)
    pres_clinton     = Clinton..Hillary.R.,
    pres_johnson     = Johnson..Gary,
    pres_stein       = Stein..Jill,
    pres_trump       = Trump..Donald.J.,
    pres_castle      = Castle..Darrell.L..,
    pres_fox         = Fox..Cherunda.L..,
    pres_kotlikoff   = Kotlikoff..Laurence.J..,
    pres_mcmullin    = McMullin..David.Evan.,
    pres_blank       = BLANK,
    pres_total       = TBC
  ) %>%
  filter(dist != "Town") %>%
  # Trim leading/trailing whitespace from municipality names before joining.
  # Both sources (MIT EDES pres, Maine SoS RCV) can have padded text fields.
  mutate(dist = str_squish(dist)) %>%
  mutate(across(pres_clinton:pres_total, parse_number)) %>%
  mutate(clinton_share = pres_clinton / pres_total)

# Trim municipality names in maine_ref as well.
# NOTE: ~38 municipalities will still be unmatched after trimming because
# the two sources name certain rural "unorganized territories" differently
# (e.g. "RANGELEY" vs "RANGELEY/ADAMSTOWN TWP"). These are kept as NA rows
# in the full_join so neither source's data is silently discarded.
maine_ref <- maine_ref %>% mutate(dist = str_squish(dist))

# Full join on municipality name AND county code. Joining on dist alone would
# produce a cross-product if two municipalities in different counties share a
# name. Both files carry county_code after the select() calls above.
# ~4-7 rows will still be unmatched due to naming differences between sources
# (e.g. "RANGELEY" vs "RANGELEY/ADAMSTOWN TWP") — those get NA for the missing side.
maine_combine <- full_join(maine_pres, maine_ref, by = c("dist", "county_code")) %>%
  mutate(
    precinct_id      = as.character(dist),
    dem_share        = clinton_share,
    yes_share        = rcv_share,
    locale           = "Maine",
    state            = "Maine",
    city             = NA_character_,
    rcv_jurisdiction = "statewide"
  )

# ============================================================================
# --- ALBANY, CA ---
# Source: https://www.acvote.org/election-information/elections?id=241#
# File: albany_all_results_2020.csv
#   One row per candidate per contest per precinct (long format).
#   Filtered to the RCV measure (Measure BB) and the presidential race, then
#   pivoted to wide format. All original columns are retained; those that are
#   constant within a contest (e.g. Contest_Id, Contest_seq_nbr) are kept but
#   will have the same value for every precinct in that contest.
# ============================================================================

albany <- read.csv("data/raw/albany_all_results_2020.csv") %>%
  rename(Precinct_name = X...Precinct_name) %>%
  select(-any_of("X"))   # drop trailing empty column produced by CSV exporter
# Note: read.csv converts spaces in column names to dots, so the raw columns
# "Election Day_ballots", "Election Night_votes", "Vote by Mail_ballots", etc.
# become "Election.Day_ballots", "Election.Night_votes", "Vote.by.Mail_ballots", etc.

# The raw file is one row per candidate per contest per precinct (long format).
# Two categories of columns require special handling before pivot_wider:
#
#   Candidate-admin columns (candidate_id, cand_seq_nbr, Candidate_Type,
#   Party_Code, Selectable_Options, Contest_party_name): internal database IDs
#   that differ between YES/NO rows but carry no analytical meaning for a
#   ballot measure. Dropped entirely.
#
#   Candidate-vote columns (total_votes and the per-voting-method vote counts):
#   these differ between the YES and NO rows for the same precinct, so they
#   cannot be id_cols in pivot_wider. They are pivoted into separate YES/NO
#   columns alongside total_votes.
#
# Contest-level columns (total_ballots, under/overvotes, method ballot counts)
# are the same for both YES and NO rows and serve as id_cols after the admin
# columns are removed.

albany_candidate_admin <- c("candidate_id", "cand_seq_nbr", "Candidate_Type",
                            "Party_Code", "Selectable_Options", "Contest_party_name")

# Candidate-specific vote columns (differ between YES and NO rows)
albany_vote_cols <- c("total_votes", "Election.Day_votes",
                      "Election.Night_votes", "Vote.by.Mail_votes")

# RCV measure (Measure BB)
# Pivot both total_votes and per-method vote counts; contest-level columns
# (ballots, under/overvotes) become id_cols since they are identical for YES/NO.
albany_measure <- albany %>%
  filter(Contest_title == "Measure BB - City of Albany") %>%
  select(-all_of(albany_candidate_admin)) %>%
  pivot_wider(
    names_from  = candidate_name,
    values_from = all_of(albany_vote_cols),
    names_glue  = "{candidate_name}_{.value}"
  ) %>%
  rename(
    rcv_yes         = YES_total_votes,
    rcv_no          = NO_total_votes,
    rcv_ed_yes      = YES_Election.Day_votes,
    rcv_ed_no       = NO_Election.Day_votes,
    rcv_en_yes      = YES_Election.Night_votes,
    rcv_en_no       = NO_Election.Night_votes,
    rcv_vbm_yes     = `YES_Vote.by.Mail_votes`,
    rcv_vbm_no      = `NO_Vote.by.Mail_votes`,
    rcv_total       = total_ballots,
    rcv_under       = total_under_votes,
    rcv_over        = total_over_votes,
    rcv_ed_ballots  = Election.Day_ballots,
    rcv_ed_under    = Election.Day_under_votes,
    rcv_ed_over     = Election.Day_over_votes,
    rcv_en_ballots  = Election.Night_ballots,
    rcv_en_under    = Election.Night_under_votes,
    rcv_en_over     = Election.Night_over_votes,
    rcv_vbm_ballots = Vote.by.Mail_ballots,
    rcv_vbm_under   = Vote.by.Mail_under_votes,
    rcv_vbm_over    = Vote.by.Mail_over_votes
  ) %>%
  # Use contested votes (YES + NO) as denominator for cross-locale consistency.
  mutate(yes_share = rcv_yes / (rcv_yes + rcv_no))

# Presidential race — same pivot structure; rename only the main candidates
albany_pres <- albany %>%
  filter(Contest_title == "President and Vice President") %>%
  select(-all_of(albany_candidate_admin)) %>%
  pivot_wider(
    names_from  = candidate_name,
    values_from = all_of(albany_vote_cols),
    names_glue  = "{candidate_name}_{.value}"
  ) %>%
  rename(
    pres_biden      = `JOSEPH R. BIDEN AND KAMALA D. HARRIS_total_votes`,
    pres_trump      = `DONALD J. TRUMP AND MICHAEL R. PENCE_total_votes`,
    pres_total      = total_ballots,
    pres_under      = total_under_votes,
    pres_over       = total_over_votes,
    pres_ed_ballots = Election.Day_ballots,
    pres_ed_under   = Election.Day_under_votes,
    pres_ed_over    = Election.Day_over_votes,
    pres_en_ballots = Election.Night_ballots,
    pres_en_under   = Election.Night_under_votes,
    pres_en_over    = Election.Night_over_votes,
    pres_vbm_ballots = Vote.by.Mail_ballots,
    pres_vbm_under  = Vote.by.Mail_under_votes,
    pres_vbm_over   = Vote.by.Mail_over_votes
  ) %>%
  mutate(biden_share = pres_biden / pres_total)

# Join measure and presidential data on precinct name.
# Precinct-level metadata (Reg_voters, Turn_Out, etc.) is identical in both
# subsets; keep it from the measure side and drop from the pres side to avoid
# .x/.y columns. All contest-specific and vote-count columns from both are kept.
albany_shared_meta <- c("Split_name", "Reporting_flag", "Update_count",
                        "Pct_Id", "Pct_seq_nbr", "Reg_voters", "Turn_Out")

albany_combine <- left_join(
  albany_measure,
  albany_pres %>% select(-all_of(albany_shared_meta)),
  by = "Precinct_name"
) %>%
  mutate(
    precinct_id      = as.character(Precinct_name),
    dem_share        = biden_share,
    locale           = "Albany",
    state            = "California",
    city             = "Albany",
    rcv_jurisdiction = "local"
  )

# ============================================================================
# --- BLOOMINGTON, MN ---
# Source (RCV): https://electionresults.sos.state.mn.us/Results/Index?
#               ersElectionId=136&scenario=ResultsByPrecinctCrosstab&QuestionId=1202
# Source (Pres): https://www.sos.state.mn.us/elections-voting/election-results/
#                2020/2020-general-election-results/2020-precinct-results-spreadsheet/
# Files: bloomington_rcv_2020.csv, minnesota_pres_2020.csv
#   The RCV file has only three columns: precinct, YES, NO.
#   The MN statewide precinct file has 62 columns covering congressional,
#   state senate, state house, and other races in addition to the presidential
#   results. All columns are retained.
# ============================================================================

bloomington_measure <- read.csv("data/raw/bloomington_rcv_2020.csv") %>%
  rename(precinct = X...County..Precinct) %>%
  filter(precinct != "Candidate Totals:") %>%   # remove the statewide totals row
  mutate(precinct = gsub("Hennepin: ", "", precinct)) %>%
  rename(rcv_yes = YES, rcv_no = NO) %>%
  mutate(across(c(rcv_yes, rcv_no), parse_number))

# Minnesota statewide precinct file — loaded once and shared with Minnetonka below
mn_pres <- read.csv("data/raw/minnesota_pres_2020.csv") %>%
  rename(precinct = PCTNAME, VTDID = X...VTDID)

bloomington_combine <- left_join(bloomington_measure, mn_pres, by = "precinct") %>%
  mutate(
    # yes_share: contested votes only (YES + NO) for cross-locale consistency.
    # dem_share: USPRSTOTAL is the total presidential votes cast in the precinct
    # (sum of all candidates), which is the correct denominator — more precise
    # than TOTVOTING (all voters across all races on the ballot).
    yes_share        = rcv_yes / (rcv_yes + rcv_no),
    dem_share        = USPRSDFL / USPRSTOTAL,
    precinct_id      = as.character(precinct),
    locale           = "Bloomington",
    state            = "Minnesota",
    city             = "Bloomington",
    rcv_jurisdiction = "local"
  )

# ============================================================================
# --- BOULDER, CO ---
# Source: https://assets.bouldercounty.org/wp-content/uploads/2020/11/
#         2020-Boulder-County-General-Election-Official-Statement-of-Votes.xlsx
# File: boulder_all_results_2020.csv
#   One row per candidate per contest per precinct (long format), similar to Albany.
#   Filtered to Question 2E (RCV) and Presidential Electors; pivoted to wide.
#   Active.Voters, undervotes, and overvotes are retained for both contests.
# ============================================================================

boulder <- read.csv("data/raw/boulder_all_results_2020.csv") %>%
  rename(Precinct.Name.Short = X...Precinct.Name..Short.)

# RCV measure (Question 2E): pivot choice → YES/FOR and NO/AGAINST columns
boulder_rcv <- boulder %>%
  filter(Contest.Name == "City of Boulder Ballot Question 2E") %>%
  select(precinct = Precinct.Name, Contest.Name, Choice.Name, Total.Votes,
         rcv_total = Total.Ballots, rcv_active_voters = Active.Voters,
         rcv_undervotes = Total.Undervotes, rcv_overvotes = Total.Overvotes) %>%
  pivot_wider(names_from = Choice.Name, values_from = Total.Votes,
              names_prefix = "rcv_choice_") %>%
  rename(
    rcv_yes = `rcv_choice_YES/FOR`,
    rcv_no  = `rcv_choice_NO/AGAINST`
  ) %>%
  # Boulder raw data has trailing spaces on numeric fields (e.g. "997 ").
  # Convert vote and ballot columns to numeric throughout.
  mutate(
    across(c(rcv_yes, rcv_no, rcv_total, rcv_active_voters, rcv_undervotes, rcv_overvotes),
           ~parse_number(as.character(.x))),
    # Use contested votes (YES + NO) as denominator for cross-locale consistency.
    yes_share = rcv_yes / (rcv_yes + rcv_no)
  )

# Presidential race: pivot choice → per-candidate vote columns
boulder_pres <- boulder %>%
  filter(Contest.Name == "Presidential Electors") %>%
  select(precinct = Precinct.Name, Contest.Name, Choice.Name, Total.Votes,
         pres_total = Total.Ballots, pres_active_voters = Active.Voters,
         pres_undervotes = Total.Undervotes, pres_overvotes = Total.Overvotes) %>%
  pivot_wider(names_from = Choice.Name, values_from = Total.Votes,
              names_prefix = "pres_") %>%
  rename(pres_biden = `pres_Joseph R. Biden / Kamala D. Harris`,
         pres_trump = `pres_Donald J. Trump / Michael R. Pence`) %>%
  # Boulder raw data has trailing spaces; convert all vote/ballot columns to numeric.
  mutate(
    across(c(pres_biden, pres_trump, pres_total, pres_active_voters,
             pres_undervotes, pres_overvotes),
           ~parse_number(as.character(.x))),
    biden_share = pres_biden / pres_total
  )

boulder_combine <- left_join(boulder_rcv, boulder_pres, by = "precinct") %>%
  mutate(
    precinct_id      = as.character(precinct),
    dem_share        = biden_share,
    locale           = "Boulder",
    state            = "Colorado",
    city             = "Boulder",
    rcv_jurisdiction = "local"
  )

# ============================================================================
# --- EUREKA, CA ---
# Source (RCV): https://humboldtgov.org/DocumentCenter/View/91079/FINAL-CANVASS-G2020-PDF
# Source (Pres): https://statewidedatabase.org/d10/g20.html
# Files: eureka_rcv_2020.csv, humboldt_pres_2020.csv
#   The RCV file covers only Eureka city precincts.
#   The Humboldt county pres file covers all county precincts and includes
#   registration totals, party registration, all races on the ballot, and
#   statewide propositions. All columns are retained. Split precincts
#   (suffix _A, indicating mail ballot vs in-person splits for the same
#   geographic precinct) are summed back to a single precinct before joining.
# ============================================================================

eureka_measure <- read.csv("data/raw/eureka_rcv_2020.csv") %>%
  rename(Precinct = X...Precinct) %>%
  filter(Precinct != "Totals") %>%   # remove the county-wide totals row
  rename(
    precinct          = Precinct,
    rcv_yes           = Yes,
    rcv_no            = No,
    rcv_cast_votes    = Cast.Votes,
    rcv_undervotes    = Undervotes,
    rcv_overvotes     = Overvotes,
    rcv_writein       = Unresolved.write.in.votes,
    rcv_vbm           = Vote.By.Mail.Ballots.Cast,
    rcv_election_day  = Election.Day.Voting.Ballots.Cast,
    rcv_early         = Early.Voting.Ballots.Cast,
    rcv_total         = Total.Ballots.Cast,
    rcv_reg_voters    = Registered.Voters,
    rcv_turnout_pct   = Turnout.Percentage
  ) %>%
  # The Totals row at the bottom of the file has comma-formatted numbers (e.g. "1,234"),
  # which causes read.csv to read those columns as character. After the Totals row is
  # filtered out the remaining values are still character strings. Convert vote-count
  # columns to numeric by stripping commas first. as.character() is applied first so
  # this is safe even if a column was already read as numeric (e.g. Overvotes = 0).
  # rcv_turnout_pct is left as character ("68.75%") since it is not used in calculations.
  mutate(across(c(rcv_yes, rcv_no, rcv_cast_votes, rcv_undervotes, rcv_overvotes,
                  rcv_writein, rcv_vbm, rcv_election_day, rcv_early,
                  rcv_total, rcv_reg_voters),
                ~as.numeric(gsub(",", "", as.character(.x))))) %>%
  # Use contested votes (YES + NO) as denominator for cross-locale consistency.
  mutate(yes_share = rcv_yes / (rcv_yes + rcv_no))

# Aggregate Humboldt split precincts: the statewide database splits each precinct
# into a base record and an "_A" record (absentee). Sum all numeric columns back
# to a single precinct row before joining to the RCV file.
eureka_pres <- read.csv("data/raw/humboldt_pres_2020.csv") %>%
  # Remove the trailing "_A" suffix that marks absentee-split records.
  # Use sub() with a $ anchor (not gsub) so only the trailing suffix is removed
  # and any "_A" that might appear elsewhere in the precinct name is untouched.
  mutate(precinct = sub("_A$", "", svprec)) %>%
  group_by(precinct) %>%
  summarize(across(where(is.numeric), sum), .groups = "drop")

eureka_combine <- left_join(eureka_measure, eureka_pres, by = "precinct") %>%
  mutate(
    dem_share        = PRSDEM01 / TOTVOTE,
    precinct_id      = as.character(precinct),
    locale           = "Eureka",
    state            = "California",
    city             = "Eureka",
    rcv_jurisdiction = "local"
  )

# ============================================================================
# --- MINNETONKA, MN ---
# Source (RCV): https://electionresults.sos.state.mn.us/Results/Index?
#               ersElectionId=136&scenario=ResultsByPrecinctCrosstab&QuestionId=1233
# Source (Pres): minnesota_pres_2020.csv (same file as Bloomington, loaded above)
# File: minnetonka_rcv_2020.csv
# ============================================================================

minnetonka_measure <- read.csv("data/raw/minnetonka_rcv_2020.csv") %>%
  rename(precinct = X...County..Precinct) %>%
  filter(precinct != "Candidate Totals:") %>%
  mutate(precinct = gsub("Hennepin: ", "", precinct)) %>%
  rename(rcv_yes = YES, rcv_no = NO) %>%
  mutate(across(c(rcv_yes, rcv_no), parse_number))

minnetonka_combine <- left_join(minnetonka_measure, mn_pres, by = "precinct") %>%
  mutate(
    # Same denominator logic as Bloomington (see comment there).
    yes_share        = rcv_yes / (rcv_yes + rcv_no),
    dem_share        = USPRSDFL / USPRSTOTAL,
    precinct_id      = as.character(precinct),
    locale           = "Minnetonka",
    state            = "Minnesota",
    city             = "Minnetonka",
    rcv_jurisdiction = "local"
  )

# ============================================================================
# --- COMBINE ALL LOCALES ---
# bind_rows() is used so every locale contributes all of its columns.
# Columns absent for a given locale receive NA for that locale's rows.
# This preserves all locale-specific variables while producing a single tidy
# dataframe that can be subsetted or analysed in full.
#
# Before binding, apply a blanket type-coercion pass to each locale dataframe:
# columns that look purely numeric (after stripping commas and whitespace) are
# converted to numeric. This resolves type conflicts that arise when the same
# column name is character in one locale (comma-formatted numbers, trailing
# spaces, or small values read as integer) and double in another.
# Columns that are intentionally character identifiers and must not be coerced
char_id_cols <- c("precinct_id", "precinct", "locale", "state", "city",
                  "rcv_jurisdiction", "svprec", "district", "dist",
                  "county_code", "rcv_turnout_pct", "PCTNAME", "PCTCODE",
                  "MCDNAME", "COUNTYNAME", "Precinct_name", "Precinct.Name")

coerce_numeric_cols <- function(df) {
  cols_to_try <- setdiff(names(df)[sapply(df, is.character)], char_id_cols)
  mutate(df, across(
    all_of(cols_to_try),
    function(x) {
      cleaned <- gsub(",", "", trimws(x))
      nums    <- suppressWarnings(as.numeric(cleaned))
      # Only convert if every originally non-NA value parsed successfully as
      # numeric (i.e. produced a non-NA result). The previous condition
      # `all(is.na(nums) == is.na(x))` was incorrect: it passed even when a
      # non-NA string like "abc" became NA after as.numeric(), which would
      # silently destroy identifier values.
      if (all(!is.na(nums[!is.na(x)]))) as.numeric(cleaned) else x
    }
  ))
}

locales <- list(ak_combine, mass_combine, maine_combine, albany_combine,
                bloomington_combine, boulder_combine, eureka_combine, minnetonka_combine)
locales <- lapply(locales, coerce_numeric_cols)

states_and_cities <- bind_rows(locales)

# ============================================================================
# --- MERGE recent_lpw ---
# A locale receives recent_lpw = TRUE if its state had a statewide race
# (Governor or U.S. Senator) won with less than 40% of the vote within 10
# years prior to the RCV ballot measure.
# The lookup joins on the state column. This means city-level locales
# (Albany, Eureka, Bloomington, Minnetonka, Boulder) inherit the LPW status
# of their state based on statewide election history — not because those
# states had statewide RCV measures (they did not in this dataset).
# ============================================================================

swe <- read.csv("data/raw/statewide_elections_lpw_2006_2018.csv") %>%
  rename(swe_state = X...State, winner_share = Winner.Share....) %>%
  select(-any_of(c("X", "X.1", "X.2", "X.3", "X.4")))  # drop trailing empty columns from CSV exporter

lpw_states <- swe %>%
  filter(winner_share < 40) %>%
  pull(swe_state) %>%
  unique()

states_and_cities <- states_and_cities %>%
  mutate(recent_lpw = state %in% lpw_states)

# ============================================================================
# --- SUMMARY ---
# ============================================================================

cat("states_and_cities created:", nrow(states_and_cities), "rows,",
    ncol(states_and_cities), "columns\n\n")

states_and_cities %>%
  group_by(locale, state, city, rcv_jurisdiction, recent_lpw) %>%
  summarise(n_precincts = n(), .groups = "drop") %>%
  print()

# Save outputs
save(states_and_cities, file = "data/rcv_data.RData")
write.csv(states_and_cities, file = "data/states_and_cities.csv", row.names = FALSE)
cat("\nSaved: data/rcv_data.RData and data/states_and_cities.csv\n")
