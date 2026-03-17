# 04_models.R
# Fits GLM/GLMM models on multiply-imputed Census-enriched RCV data.
# Pools results across 5 imputations using mice::pool().
#
# Input:  data/rcv_data_mids.RData (mids object from 03_imputation.R)
#         data/rcv_data_census.RData (observed data with outcome variables)
# Output: data/model_results.RData
#
# Models:
#   m1: cbind(rcv_yes, rcv_no) ~ dem_share + recent_lpw      (quasibinom baseline)
#   m2: cbind(rcv_yes, rcv_no) ~ dem_share + recent_lpw + X  (quasibinom + controls)
#   m3: cbind(rcv_yes, rcv_no) ~ ... + (1 | locale)          (GLMM + controls)

library(mice)
library(dplyr)
library(lme4)
library(broom)
library(broom.mixed)

# --- Load and reconstruct full imputed datasets ---
load("data/rcv_data_mids.RData")   # mids + analysis_rows
load("data/rcv_data_census.RData")

long <- complete(mids, action = "long", include = TRUE)

# Use analysis_rows (saved in 03_imputation.R) to align outcome columns with
# the imputation dataframe. The mids object was built on a subset of rows
# (those with non-NA dem_share), so we must index the census data accordingly.
census_subset <- states_and_cities_census[analysis_rows, ]
attach_cols <- c("yes_share", "rcv_yes", "rcv_no", "precinct_id",
                 "locale", "state", "rcv_jurisdiction")
for (col in attach_cols) {
  long[[col]] <- rep(census_subset[[col]], times = mids$m + 1)
}

full_mids <- as.mids(long)

# --- Helper: filter to analysis sample ---
filter_analysis <- function(d) {
  d %>%
    filter(!is.na(yes_share), !is.na(dem_share),
           !is.na(rcv_yes), !is.na(rcv_no),
           (rcv_yes + rcv_no) > 0) %>%
    mutate(n_votes = rcv_yes + rcv_no)
}

demo_controls <- "pct_white_vap + pct_black_vap + pct_hispanic_vap + median_income + pct_bach_plus + pct_renter"

cat("=== Fitting models across", mids$m, "imputations ===\n\n")

# ============================================================================
# Model 1: Quasibinomial baseline (no demographic controls)
# ============================================================================

cat("--- M1: Quasibinomial baseline ---\n")
fit_m1_list <- lapply(1:mids$m, function(i) {
  d <- filter_analysis(complete(full_mids, i))
  glm(cbind(rcv_yes, rcv_no) ~ dem_share + recent_lpw,
      family = quasibinomial(link = "logit"), data = d)
})
fit_m1 <- list(analyses = fit_m1_list)
class(fit_m1) <- c("mira", "list")
pool_m1 <- pool(fit_m1)

# ============================================================================
# Model 2: Quasibinomial with demographic controls
# ============================================================================

cat("--- M2: Quasibinomial + demographic controls ---\n")
fit_m2_list <- lapply(1:mids$m, function(i) {
  d <- filter_analysis(complete(full_mids, i))
  glm(as.formula(paste("cbind(rcv_yes, rcv_no) ~ dem_share + recent_lpw +", demo_controls)),
      family = quasibinomial(link = "logit"), data = d)
})
fit_m2 <- list(analyses = fit_m2_list)
class(fit_m2) <- c("mira", "list")
pool_m2 <- pool(fit_m2)

# ============================================================================
# Model 3: GLMM with random intercept by locale
# ============================================================================

cat("--- M3: GLMM (random intercept by locale) + controls ---\n")
fit_m3_list <- lapply(1:mids$m, function(i) {
  d <- filter_analysis(complete(full_mids, i))
  glmer(
    as.formula(paste("cbind(rcv_yes, rcv_no) ~ dem_share + recent_lpw +",
                     demo_controls, "+ (1 | locale)")),
    family = binomial,
    data = d,
    nAGQ = 10,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
  )
})
fit_m3 <- list(analyses = fit_m3_list)
class(fit_m3) <- c("mira", "list")
pool_m3 <- pool(fit_m3)

cat("Done.\n")

# ============================================================================
# Model comparison metrics
# ============================================================================

cat("\n=== Model comparison (averaged across imputations) ===\n")

# Deviance pseudo-R²: 1 - deviance(model) / deviance(null)
# Adjusted: 1 - (deviance/df.residual) / (null.deviance/df.null)
compute_pseudo_r2 <- function(model_list) {
  raw <- sapply(model_list, function(m) 1 - deviance(m) / m$null.deviance)
  adj <- sapply(model_list, function(m) {
    1 - (deviance(m) / df.residual(m)) / (m$null.deviance / m$df.null)
  })
  list(pseudo_r2 = mean(raw), adj_pseudo_r2 = mean(adj))
}

pr2_m1 <- compute_pseudo_r2(fit_m1_list)
pr2_m2 <- compute_pseudo_r2(fit_m2_list)

cat(sprintf("  M1 (baseline):     adj. pseudo-R² = %.4f\n", pr2_m1$adj_pseudo_r2))
cat(sprintf("  M2 (+ controls):   adj. pseudo-R² = %.4f\n", pr2_m2$adj_pseudo_r2))

# Overdispersion
cat(sprintf("\n  M1 overdispersion: %.1f\n",
            mean(sapply(fit_m1_list, function(m) deviance(m)/df.residual(m)))))
cat(sprintf("  M2 overdispersion: %.1f\n",
            mean(sapply(fit_m2_list, function(m) deviance(m)/df.residual(m)))))

# F-test: M1 vs M2 (nested)
cat("\n--- F-test: M1 vs M2 (are demographic controls significant?) ---\n")
f_tests <- lapply(1:mids$m, function(i) anova(fit_m1_list[[i]], fit_m2_list[[i]], test = "F"))
cat("p-values:", paste(sapply(f_tests, function(a) format.pval(a$`Pr(>F)`[2], digits=3)), collapse=", "), "\n")

# ============================================================================
# Save
# ============================================================================

diag_data <- filter_analysis(complete(full_mids, 1))
diag_m2 <- fit_m2_list[[1]]

save(pool_m1, pool_m2, pool_m3,
     fit_m1, fit_m2, fit_m3,
     pr2_m1, pr2_m2,
     diag_data, diag_m2,
     file = "data/model_results.RData")

cat("\nSaved: data/model_results.RData\n")

cat("\n=== Pooled Results ===\n\n")
cat("--- M1: Quasibinomial baseline ---\n")
print(summary(pool_m1))
cat("\n--- M2: Quasibinomial + demographics ---\n")
print(summary(pool_m2))
cat("\n--- M3: GLMM + demographics ---\n")
print(summary(pool_m3))
