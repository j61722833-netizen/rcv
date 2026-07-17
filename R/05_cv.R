# 05_cv.R
# Out-of-sample cross-validation of the RCV support models.
#
# Input:  data/rcv_data_mids.RData   (mids + analysis_rows from 03_imputation.R)
#         data/rcv_data_census.RData (states_and_cities_census, observed outcomes)
# Output: data/cv_results.RData      (cv_results list + tidy data frames + provenance)
#         data/cv_metrics.csv        (pooled OOS metrics, one row per model)
#         data/cv_fold_metrics.csv   (per-held-out-locale metrics, long format)
#
# ============================================================================
# WHY THIS EXISTS
# ============================================================================
# The in-sample adjusted pseudo-R2 (0.86 / 0.83) in 04_models.R measures fit to
# the SAME precincts the models were trained on. It cannot answer the question a
# reader actually cares about: given a *new* jurisdiction, how well would these
# models predict its precinct-level RCV support? This script answers that with a
# genuinely out-of-sample evaluation.
#
# ============================================================================
# DESIGN DECISIONS (the honest bits)
# ============================================================================
# (1) GROUPING: Leave-One-Locale-Out (LOLO), 8 folds.
#     Precincts are nested within 8 locales. Locale drives almost everything:
#     the outcome (RCV support), the key regressor `recent_lpw` (a locale-level
#     constant), and M3's random intercept. A naive random precinct split would
#     let the model memorise each locale's mean from its own training precincts
#     and then "predict" its held-out precincts -- leaking exactly the cluster
#     structure we want to test generalisation across. So each fold holds out one
#     ENTIRE locale and predicts it from the other seven. This is the correct,
#     harsh test of "generalise to a new jurisdiction," and it can (honestly)
#     yield a negative deviance-R2 if a model does worse than an intercept.
#
#     Note on identifiability: only Alaska and Maine have recent_lpw = TRUE. LOLO
#     keeps recent_lpw identified in every fold (holding out one TRUE locale still
#     leaves the other). Holding out Alaska/Maine is therefore a direct
#     out-of-sample test of the paper's central LPW hypothesis: does the LPW shift
#     estimated from Maine alone actually predict Alaska, and vice versa?
#
# (2) IMPUTATION UNDER CV (leakage-aware):
#     - M1 uses only dem_share * recent_lpw -> NO demographics -> NO imputation.
#       Its CV is fully leakage-free. This is the cleanest headline number.
#     - M2 / M3 use 6 Census demographics with missing values. We REFIT the mice
#       imputation model *inside each training fold* using mice's `ignore=`
#       argument: the held-out locale's rows are imputed but never contribute to
#       estimating the imputation model. No held-out information leaks into the
#       imputed predictors. Compromise, dlog'd: we drop `locale` from the
#       imputation predictor set here (03_imputation.R used it), because under
#       LOLO the held-out locale is an unseen factor level -- a training-fold
#       imputation model cannot form a coefficient for it. Demographics are
#       imputed from dem_share + recent_lpw + the other demographics (pmm). This
#       makes held-out imputation regress toward training patterns, which is the
#       honest reality of predicting a new jurisdiction's missing demographics.
#
# (3) METRICS (appropriate to a quasibinomial / proportion outcome):
#     - OOS deviance-explained: 1 - sum(binomial deviance under model)
#                                   / sum(binomial deviance under null),
#       where the null predicts each fold's own training-set overall support rate.
#       Pooled over all held-out precincts. The direct out-of-sample analog of the
#       in-sample pseudo-R2.
#     - MAE / RMSE of predicted support share (percentage points), precinct-level
#       (unweighted) and vote-weighted. The interpretable "how far off" number.
#     All metrics pooled across the 5 imputations by AVERAGING the predicted
#     probabilities per precinct (mice's recommended prediction pooling), then
#     computing the metric once. Across-imputation spread is also reported.
#
# ============================================================================

library(mice)
library(dplyr)
library(lme4)

set.seed(2024)  # fixed: CV fold assignment is deterministic (by locale) but mice draws are not

# ============================================================================
# Load + build the analysis base table (mirrors 04_models.R filter_analysis)
# ============================================================================

load("data/rcv_data_mids.RData")    # mids, analysis_rows
load("data/rcv_data_census.RData")  # states_and_cities_census

demo_vars <- c("pct_white_vap", "pct_black_vap", "pct_hispanic_vap",
               "median_income", "pct_bach_plus", "pct_renter")
demo_controls <- paste(demo_vars, collapse = " + ")

# Same analysis sample as 04_models.R: dem_share-complete subset, valid outcome,
# positive vote totals. Demographics may still be NA here (imputed per-fold below).
base <- states_and_cities_census[analysis_rows, ] %>%
  filter(!is.na(yes_share), !is.na(dem_share),
         !is.na(rcv_yes), !is.na(rcv_no),
         (rcv_yes + rcv_no) > 0) %>%
  mutate(recent_lpw = as.numeric(recent_lpw),
         n_votes    = rcv_yes + rcv_no) %>%
  select(all_of(c("locale", "rcv_yes", "rcv_no", "yes_share", "dem_share",
                  "recent_lpw", "n_votes", demo_vars))) %>%
  mutate(row_id = row_number())   # stable precinct id: pool predictions by this, not by value

stopifnot(nrow(base) > 0, !any(is.na(base$dem_share)), !any(is.na(base$recent_lpw)))

locales <- sort(unique(base$locale))
n_folds <- length(locales)
M_IMP   <- mids$m       # 5, match the pipeline
MAXIT   <- 20L          # match 03_imputation.R (mids object does not store maxit)

cat(sprintf("=== Leave-one-locale-out CV: %d folds, %d precincts, %d imputations ===\n\n",
            n_folds, nrow(base), M_IMP))
cat("Fold (held-out locale) sizes:\n")
print(table(base$locale))
cat("\n")

# ============================================================================
# Metric helpers
# ============================================================================

# Binomial deviance per precinct (the quantity glm minimises), robust at 0/1.
bin_deviance <- function(y, n, p) {
  p  <- pmin(pmax(p, 1e-9), 1 - 1e-9)
  mu <- n * p
  t1 <- ifelse(y == 0,     0, y * log(y / mu))
  t2 <- ifelse(n - y == 0, 0, (n - y) * log((n - y) / (n - mu)))
  2 * (t1 + t2)
}

# ============================================================================
# CV drivers. Each returns a data frame of held-out predictions with one row
# per (imputation, precinct): locale, y, n, obs (=y/n), p (predicted), imp, null_p.
# ============================================================================

# ---- M1: dem_share * recent_lpw, quasibinomial, NO imputation -------------
cv_m1 <- function() {
  out <- vector("list", n_folds)
  for (k in seq_along(locales)) {
    L    <- locales[k]
    tr   <- base[base$locale != L, ]
    te   <- base[base$locale == L, ]
    fit  <- glm(cbind(rcv_yes, rcv_no) ~ dem_share * recent_lpw,
                family = quasibinomial("logit"), data = tr)
    null_p <- sum(tr$rcv_yes) / sum(tr$n_votes)   # intercept-only competitor
    p    <- predict(fit, newdata = te, type = "response")
    out[[k]] <- data.frame(row_id = te$row_id, locale = L, y = te$rcv_yes,
                           n = te$n_votes, obs = te$yes_share, p = p,
                           null_p = null_p, imp = 1L)
  }
  bind_rows(out)
}

# ---- Shared: leakage-safe in-fold imputation for M2 / M3 -------------------
# Returns, for a given held-out locale, a list of M_IMP completed base tables
# (demographics imputed) with median_income scaled to $10k units as in 04.
impute_fold <- function(L) {
  ig <- base$locale == L                       # held-out rows: imputed, not trained on
  imp_in <- base[, c(demo_vars, "dem_share", "recent_lpw")]
  ini  <- mice(imp_in, maxit = 0, printFlag = FALSE)
  meth <- ini$method
  meth[c("dem_share", "recent_lpw")] <- ""     # predictors, never imputed
  meth[demo_vars] <- "pmm"
  mids_k <- mice(imp_in, m = M_IMP, maxit = MAXIT, method = meth,
                 ignore = ig, printFlag = FALSE)
  lapply(seq_len(M_IMP), function(i) {
    comp <- complete(mids_k, i)
    d <- base
    d[demo_vars] <- comp[demo_vars]
    d$median_income <- d$median_income / 10000  # match 04_models.R scaling
    d
  })
}

# ---- M2: + demographic controls, quasibinomial ----------------------------
cv_m2 <- function() {
  f <- as.formula(paste("cbind(rcv_yes, rcv_no) ~ dem_share * recent_lpw +", demo_controls))
  out <- list()
  for (k in seq_along(locales)) {
    L     <- locales[k]
    comps <- impute_fold(L)
    for (i in seq_len(M_IMP)) {
      d      <- comps[[i]]
      tr     <- d[d$locale != L, ]
      te     <- d[d$locale == L, ]
      fit    <- glm(f, family = quasibinomial("logit"), data = tr)
      null_p <- sum(tr$rcv_yes) / sum(tr$n_votes)
      p      <- predict(fit, newdata = te, type = "response")
      out[[length(out) + 1]] <- data.frame(
        row_id = te$row_id, locale = L, y = te$rcv_yes, n = te$n_votes,
        obs = te$yes_share, p = p, null_p = null_p, imp = i)
    }
    cat(sprintf("  M2 fold %d/%d (%s) done\n", k, n_folds, L))
  }
  bind_rows(out)
}

# ---- M3: GLMM random intercept by locale; held-out RE unknown -> RE=0 ------
cv_m3 <- function() {
  f <- as.formula(paste("cbind(rcv_yes, rcv_no) ~ dem_share * recent_lpw +",
                        demo_controls, "+ (1 | locale)"))
  out <- list()
  for (k in seq_along(locales)) {
    L     <- locales[k]
    comps <- impute_fold(L)
    for (i in seq_len(M_IMP)) {
      d   <- comps[[i]]
      tr  <- d[d$locale != L, ]
      te  <- d[d$locale == L, ]
      fit <- glmer(f, family = binomial, data = tr, nAGQ = 1,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 100000)))
      null_p <- sum(tr$rcv_yes) / sum(tr$n_votes)
      # New locale => no known random intercept => population-level prediction.
      p <- predict(fit, newdata = te, type = "response", re.form = NA,
                   allow.new.levels = TRUE)
      out[[length(out) + 1]] <- data.frame(
        row_id = te$row_id, locale = L, y = te$rcv_yes, n = te$n_votes,
        obs = te$yes_share, p = p, null_p = null_p, imp = i)
    }
    cat(sprintf("  M3 fold %d/%d (%s) done\n", k, n_folds, L))
  }
  bind_rows(out)
}

# ============================================================================
# Pool predictions across imputations, then score.
# ============================================================================

# Average predicted p across the M imputations for each held-out precinct, so
# each held-out precinct appears once with an imputation-integrated prediction.
pool_predictions <- function(preds) {
  preds %>%
    group_by(row_id, locale, y, n, obs, null_p) %>%   # by precinct id, not by value
    summarise(p = mean(p), .groups = "drop")
}

# Pooled (all held-out precincts) metrics on imputation-averaged predictions.
score_pooled <- function(pp) {
  dev_model <- sum(bin_deviance(pp$y, pp$n, pp$p))
  dev_null  <- sum(bin_deviance(pp$y, pp$n, pp$null_p))
  ae <- abs(pp$obs - pp$p)
  data.frame(
    oos_dev_r2      = 1 - dev_model / dev_null,
    mae             = mean(ae),
    rmse            = sqrt(mean((pp$obs - pp$p)^2)),
    mae_vote_wt     = sum(pp$n * ae) / sum(pp$n),
    n_precincts     = nrow(pp)
  )
}

# Per-held-out-locale metrics (each locale scored on its own precincts).
score_by_fold <- function(pp) {
  pp %>%
    group_by(locale) %>%
    summarise(
      n_precincts = n(),
      oos_dev_r2  = 1 - sum(bin_deviance(y, n, p)) / sum(bin_deviance(y, n, null_p)),
      mae         = mean(abs(obs - p)),
      rmse        = sqrt(mean((obs - p)^2)),
      mean_obs    = mean(obs),
      mean_pred   = mean(p),
      .groups = "drop"
    )
}

# Spread of the headline metrics across individual imputations (before pooling),
# to show how much imputation uncertainty moves the number. M1 has no imputation.
score_across_imp <- function(preds) {
  preds %>%
    group_by(imp) %>%
    summarise(
      oos_dev_r2 = 1 - sum(bin_deviance(y, n, p)) / sum(bin_deviance(y, n, null_p)),
      mae        = mean(abs(obs - p)),
      .groups = "drop"
    ) %>%
    summarise(
      dev_r2_mean = mean(oos_dev_r2), dev_r2_sd = sd(oos_dev_r2),
      mae_mean    = mean(mae),        mae_sd    = sd(mae),
      m           = n()
    )
}

# ============================================================================
# Run all three
# ============================================================================

cat("--- M1 (baseline, no imputation) ---\n")
preds_m1 <- cv_m1()
cat("--- M2 (+ demographic controls, in-fold imputation) ---\n")
preds_m2 <- cv_m2()
cat("--- M3 (GLMM, RE=0 for held-out locale, in-fold imputation) ---\n")
preds_m3 <- cv_m3()

pp_m1 <- pool_predictions(preds_m1)
pp_m2 <- pool_predictions(preds_m2)
pp_m3 <- pool_predictions(preds_m3)

metrics <- bind_rows(
  cbind(model = "M1 baseline (dem_share * recent_lpw)",  score_pooled(pp_m1)),
  cbind(model = "M2 + demographics",                     score_pooled(pp_m2)),
  cbind(model = "M3 GLMM (+ demographics)",              score_pooled(pp_m3))
)

fold_metrics <- bind_rows(
  cbind(model = "M1", score_by_fold(pp_m1)),
  cbind(model = "M2", score_by_fold(pp_m2)),
  cbind(model = "M3", score_by_fold(pp_m3))
)

imp_spread <- bind_rows(
  cbind(model = "M1", score_across_imp(preds_m1)),
  cbind(model = "M2", score_across_imp(preds_m2)),
  cbind(model = "M3", score_across_imp(preds_m3))
)

# Fold-mean (each held-out locale weighted equally) alongside precinct-pooled.
fold_mean <- fold_metrics %>%
  group_by(model) %>%
  summarise(mae_fold_mean = mean(mae), dev_r2_fold_mean = mean(oos_dev_r2),
            .groups = "drop")

# ============================================================================
# Report
# ============================================================================

cat("\n=========================================================\n")
cat("OUT-OF-SAMPLE (leave-one-locale-out) METRICS -- POOLED\n")
cat("=========================================================\n")
for (i in seq_len(nrow(metrics))) {
  r <- metrics[i, ]
  cat(sprintf("%-38s  OOS dev-R2 = %+.3f | MAE = %.3f (%.1f pp) | RMSE = %.3f | vote-wt MAE = %.3f\n",
              r$model, r$oos_dev_r2, r$mae, 100 * r$mae, r$rmse, r$mae_vote_wt))
}

cat("\n--- Precinct-pooled vs fold-mean (equal weight per locale) ---\n")
print(merge(
  metrics[, c("model", "oos_dev_r2", "mae")],
  transform(fold_mean, model = c("M1 baseline (dem_share * recent_lpw)",
                                 "M2 + demographics", "M3 GLMM (+ demographics)")),
  by = "model"), row.names = FALSE)

cat("\n--- Per-held-out-locale (M1 baseline) ---\n")
print(fold_metrics[fold_metrics$model == "M1",
                   c("locale", "n_precincts", "oos_dev_r2", "mae", "mean_obs", "mean_pred")],
      row.names = FALSE, digits = 3)

cat("\n--- Imputation-uncertainty spread of headline metrics ---\n")
print(imp_spread, row.names = FALSE, digits = 3)

# ============================================================================
# Persist with provenance
# ============================================================================

provenance <- list(
  script       = "R/05_cv.R",
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  r_version    = R.version.string,
  mice_version = as.character(packageVersion("mice")),
  lme4_version = as.character(packageVersion("lme4")),
  git_sha      = tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                          error = function(e) NA_character_),
  cv_design    = "leave-one-locale-out",
  n_folds      = n_folds,
  n_precincts  = nrow(base),
  m_imputations = M_IMP,
  maxit        = MAXIT,
  seed         = 2024,
  null_model   = "intercept-only (training-fold overall support rate)",
  pooling      = "predicted probabilities averaged across imputations, then scored"
)

cv_results <- list(
  metrics      = metrics,
  fold_metrics = fold_metrics,
  fold_mean    = fold_mean,
  imp_spread   = imp_spread,
  pooled_preds = list(M1 = pp_m1, M2 = pp_m2, M3 = pp_m3),
  provenance   = provenance
)

save(cv_results, file = "data/cv_results.RData")
write.csv(cbind(metrics, oos_design = "leave-one-locale-out",
                generated_at = provenance$generated_at,
                git_sha = provenance$git_sha),
          "data/cv_metrics.csv", row.names = FALSE)
write.csv(fold_metrics, "data/cv_fold_metrics.csv", row.names = FALSE)

cat("\nSaved:\n")
cat("  data/cv_results.RData     (cv_results list + provenance)\n")
cat("  data/cv_metrics.csv       (pooled OOS metrics)\n")
cat("  data/cv_fold_metrics.csv  (per-locale metrics)\n")
cat(sprintf("\nProvenance: %s | R %s | mice %s | git %s\n",
            provenance$generated_at, getRversion(), provenance$mice_version,
            provenance$git_sha))
