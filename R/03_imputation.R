# 03_imputation.R
# Multiple imputation of missing Census demographics using mice.
#
# Input:  data/rcv_data_census.RData (states_and_cities_census)
# Output: data/rcv_data_imputed.RData (states_and_cities_imputed, single imputed dataset)
#         data/rcv_data_mids.RData (mids object for pooled analysis with mice::pool)
#
# Strategy:
#   - Impute only the 6 Census demographic variables
#   - Exclude ALL RCV outcome variables (yes_share, rcv_yes, rcv_no, etc.)
#     to prevent leaking the dependent variable into imputed predictors
#   - Include dem_share (a predictor in the analysis model) and locale
#     to help the imputation model predict demographics accurately
#   - recent_lpw is included as it is a predictor in the analysis model
#
# Missingness summary (from 02_census_api.R):
#   PL race vars: ~52 NA (1.5%) — Maine unorganized territories, a few MA/AK
#   ACS vars: ~400 NA (11.3%) — rural AK, ME townships, some MA towns
#   Pattern is geographically structured (rural, small places)

library(mice)
library(dplyr)

# --- Load data ---
load("data/rcv_data_census.RData")

# --- Build imputation dataframe ---
# Only include variables relevant to the imputation model:
#   - Variables to impute (census demographics)
#   - Predictors that help predict the missing values
#   - NO RCV outcome variables (yes_share, rcv_yes, rcv_no, etc.)

vars_to_impute <- c("pct_white_vap", "pct_black_vap", "pct_hispanic_vap",
                     "median_income", "pct_bach_plus", "pct_renter")

# Predictors for the imputation model
vars_predictors <- c("dem_share", "locale", "recent_lpw")

imp_df <- states_and_cities_census %>%
  select(all_of(c(vars_to_impute, vars_predictors))) %>%
  # locale as factor for mice
  mutate(locale = as.factor(locale),
         recent_lpw = as.numeric(recent_lpw))

# --- Diagnostics before imputation ---
cat("=== Missingness summary ===\n")
for (v in vars_to_impute) {
  n_na <- sum(is.na(imp_df[[v]]))
  cat(sprintf("  %-18s %4d NA (%5.1f%%)\n", v, n_na, 100 * n_na / nrow(imp_df)))
}
cat(sprintf("  %-18s %4d NA (%5.1f%%)\n", "dem_share",
            sum(is.na(imp_df$dem_share)), 100 * sum(is.na(imp_df$dem_share)) / nrow(imp_df)))

# Drop rows where dem_share is NA — these can't contribute to imputation
# (8 rows with missing presidential data, mostly Maine unmatched municipalities)
rows_before <- nrow(imp_df)
complete_predictor_mask <- !is.na(imp_df$dem_share)
cat("\nRows with dem_share NA (excluded from imputation):", sum(!complete_predictor_mask), "\n")
analysis_rows <- which(complete_predictor_mask)
imp_df <- imp_df[complete_predictor_mask, ]
cat("Rows after filtering:", nrow(imp_df), "\n")

# --- Set up mice ---
cat("\n=== Running mice ===\n")

# Default methods: pmm (predictive mean matching) for continuous variables,
# which preserves the distributional properties and bounds (e.g. proportions
# stay in [0,1] without explicit constraints).
# logreg for binary, polyreg for unordered categorical.

# Set up the predictor matrix: control which variables predict which
ini <- mice(imp_df, maxit = 0)
pred <- ini$predictorMatrix
meth <- ini$method

# Don't impute the predictors (dem_share, locale, recent_lpw)
meth[vars_predictors] <- ""

# All vars_to_impute use pmm (default for numeric, good for bounded proportions)
# mice will have set this already, but be explicit
meth[vars_to_impute] <- "pmm"

cat("Methods:\n")
print(meth[meth != ""])
cat("\nPredictor matrix (rows = imputed, cols = predictors):\n")
print(pred[vars_to_impute, ])

# Run mice: 5 imputations, 20 iterations
set.seed(42)
mids <- mice(imp_df, m = 5, maxit = 20, method = meth,
             predictorMatrix = pred, printFlag = FALSE)

cat("\nMice completed: m =", mids$m, ", maxit =", mids$maxit, "\n")

# --- Diagnostics after imputation ---
cat("\n=== Convergence check ===\n")
cat("Inspect convergence with plot(mids) — traces should mix well.\n")
cat("Logged events:", nrow(mids$loggedEvents), "\n")
if (nrow(mids$loggedEvents) > 0) {
  print(head(mids$loggedEvents, 10))
}

# --- Create single imputed dataset (first imputation) ---
# For exploratory analysis. For final inference, use mice::pool().
imp_complete <- complete(mids, action = 1)

# Verify no NAs remain in imputed variables
cat("\n=== Post-imputation missingness ===\n")
for (v in vars_to_impute) {
  n_na <- sum(is.na(imp_complete[[v]]))
  cat(sprintf("  %-18s %d NA\n", v, n_na))
}

# --- Merge imputed values back into full dataset ---
# Only overwrite NA values in the original; keep observed values unchanged.
states_and_cities_imputed <- states_and_cities_census

for (v in vars_to_impute) {
  # imp_complete has nrow(imp_df) rows (dem_share-complete subset), indexed 1:N.
  # states_and_cities_imputed has the full dataset. analysis_rows maps each
  # imp_complete row back to the correct full-dataset position.
  full_na_idx <- which(is.na(states_and_cities_imputed[[v]]) &
                         seq_len(nrow(states_and_cities_imputed)) %in% analysis_rows)
  imp_positions <- match(full_na_idx, analysis_rows)
  states_and_cities_imputed[[v]][full_na_idx] <- imp_complete[[v]][imp_positions]
}

cat("\n=== Final dataset summary ===\n")
cat("Rows:", nrow(states_and_cities_imputed), "\n")
cat("Remaining NAs in imputed vars:\n")
for (v in vars_to_impute) {
  cat(sprintf("  %-18s %d\n", v, sum(is.na(states_and_cities_imputed[[v]]))))
}

# Summary stats of imputed variables by locale
cat("\n=== Imputed variable means by locale ===\n")
states_and_cities_imputed %>%
  group_by(locale) %>%
  summarise(
    n = n(),
    pct_white    = round(mean(pct_white_vap, na.rm = TRUE), 3),
    pct_black    = round(mean(pct_black_vap, na.rm = TRUE), 3),
    pct_hispanic = round(mean(pct_hispanic_vap, na.rm = TRUE), 3),
    med_income   = round(mean(median_income, na.rm = TRUE), 0),
    pct_bach     = round(mean(pct_bach_plus, na.rm = TRUE), 3),
    pct_rent     = round(mean(pct_renter, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print()

# --- Save ---
save(states_and_cities_imputed, file = "data/rcv_data_imputed.RData")
save(mids, analysis_rows, file = "data/rcv_data_mids.RData")
write.csv(states_and_cities_imputed, file = "data/states_and_cities_imputed.csv",
          row.names = FALSE)

cat("\nSaved:\n")
cat("  data/rcv_data_imputed.RData  — single imputed dataset (imputation 1)\n")
cat("  data/rcv_data_mids.RData     — mids object (5 imputations, for mice::pool)\n")
cat("  data/states_and_cities_imputed.csv\n")
cat("\nFor pooled regression, see R/04_models.R\n")
