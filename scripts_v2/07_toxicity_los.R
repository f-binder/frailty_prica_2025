# ==============================================================================
# 07_toxicity_los.R
#
# Purpose : Do baseline frailty measures predict
#   A. acute toxicity (CRS grade >=2, any ICANS, ICANS grade >=2) and ICU
#      admission?  -> univariable logistic regression (few events: no MVA)
#   B. length of the index (infusion) admission? -> negative binomial
#      regression (rate ratio per unit), unadjusted and adjusted for age and
#      log2 LDH; prolonged stay (>14 days) by CFS group, logistic and AUC;
#      sensitivity analyses without the most extreme patients.
# Inputs  : data/processed/v2/analysis_wide.rds
# Outputs : output/v2/tables/toxicity_logistic.csv, los_models.csv, los_by_cfs.csv
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "07_toxicity_los.R"
section("07 - Toxicity, ICU admission and length of stay")

d <- read_proc("analysis_wide")

frailty_predictors <- c(
  cfs_score = "CFS (per point)", cfs_cat = "CFS >3", ves13_score = "VES-13 (per point)",
  ves_cat = "VES-13 >=3", walk_mpers = "Gait speed (per m/s)", gait_cat = "Gait <=0.8 m/s",
  gait_slow_nd = "Gait slow or not done",
  setNames(c(grip_z = "Grip strength (per SD vs norms)", grip_pctpred = "Grip strength (per % predicted)",
             grip_average_kg = "Grip strength (per kg)")[[GRIP_VAR]], GRIP_VAR),
  minicog_score = "Mini-Cog (per point)", phq_total_score = "PHQ-9 (per point)",
  hctci_score = "HCT-CI (per point)", cirs_total = "CIRS (per point)"
)
other_predictors <- c(age = "Age (per year)", ecog = "ECOG (per point)", log2_ldh = "LDH (per doubling)")
all_predictors <- c(frailty_predictors, other_predictors)

## ---- A. Toxicity and ICU: univariable logistic regression ------------------------------

tox_outcomes <- c(crs_g2 = "CRS grade >=2", icans_any = "ICANS (any)", icans_g2 = "ICANS grade >=2", icu = "ICU admission")

message(sprintf("Events: CRS >=2 %d/%d, ICANS %d, ICANS >=2 %d, ICU %d",
                sum(d$crs_g2), nrow(d), sum(d$icans_any), sum(d$icans_g2), sum(d$icu)))

fit_logistic <- function(outcome, predictor) {
  warn <- character()
  fit <- withCallingHandlers(glm(as.formula(paste(outcome, "~", predictor)), data = d, family = binomial),
                             warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") })
  cf <- coef(summary(fit))[2, ]
  ci <- exp(cf[1] + c(-1, 1) * 1.96 * cf[2])   # Wald CI (profile CIs fail under separation)
  tibble(outcome = tox_outcomes[outcome], predictor = all_predictors[predictor],
         or = exp(cf[1]), lo = ci[1], hi = ci[2], p = cf[4], n = nobs(fit), events = sum(fit$y),
         separation = any(str_detect(warn, "fitted probabilities numerically 0 or 1")))
}

toxicity <- expand_grid(outcome = names(tox_outcomes), predictor = names(all_predictors)) %>%
  pmap_dfr(fit_logistic)
save_table(toxicity %>% mutate(across(c(or, lo, hi), ~ round(.x, 2))), "toxicity_logistic")

message("\nUnivariable associations with p < 0.10 (all others p >= 0.10) ", SIG_LEGEND, ":")
print(toxicity %>% filter(p < 0.10) %>% mutate(across(c(or, lo, hi), ~ round(.x, 2)), p = fmt_p_star(p)))
message("Fits flagged for (quasi-)separation (OR not interpretable): ",
        paste(unique(paste(toxicity$outcome[toxicity$separation], toxicity$predictor[toxicity$separation], sep = " ~ ")), collapse = "; "))

frailty_tox <- toxicity %>% filter(predictor %in% frailty_predictors, !separation)
record("tox_min_p_frailty", min(frailty_tox$p),
       note = "smallest p, any frailty measure vs CRS>=2 / ICANS / ICANS>=2 / ICU")
cfs_tox <- toxicity %>% filter(predictor == "CFS (per point)")
for (i in seq_len(nrow(cfs_tox))) {
  record(paste0("tox_or_cfs_", names(tox_outcomes)[match(cfs_tox$outcome[i], tox_outcomes)]),
         cfs_tox$or[i], lo = cfs_tox$lo[i], hi = cfs_tox$hi[i], p = cfs_tox$p[i])
}

## ---- B. Length of index admission ------------------------------------------------------

message(sprintf("\nLength of stay: median %g days (IQR %g-%g, range %g-%g); >%d days in %d patients (%.0f%%)",
                median(d$los_days), quantile(d$los_days, 0.25), quantile(d$los_days, 0.75),
                min(d$los_days), max(d$los_days), LOS_PROLONGED, sum(d$los_prolonged), 100 * mean(d$los_prolonged)))

fit_nb <- function(predictor, adjust = character(), data = d, label = "") {
  rhs <- paste(c(predictor, adjust), collapse = " + ")
  fit <- MASS::glm.nb(as.formula(paste("los_days ~", rhs)), data = data)
  cf <- coef(summary(fit))
  rows <- rownames(cf)[-1][!rownames(cf)[-1] %in% unlist(lapply(adjust, function(a) grep(paste0("^", a), rownames(cf), value = TRUE)))]
  map_dfr(rows, function(r) tibble(
    predictor = coalesce(unname(all_predictors[predictor]), predictor), term = r,
    adjusted = if (length(adjust)) paste(adjust, collapse = " + ") else "none",
    analysis = label, rr = exp(cf[r, 1]),
    lo = exp(cf[r, 1] - 1.96 * cf[r, 2]), hi = exp(cf[r, 1] + 1.96 * cf[r, 2]),
    p = cf[r, 4], n = nobs(fit)))
}

los_models <- bind_rows(
  map_dfr(names(all_predictors), ~ fit_nb(.x, label = "unadjusted")),
  map_dfr(names(frailty_predictors), ~ fit_nb(.x, adjust = ADJ, label = "adjusted")),
  # sensitivity analyses for CFS
  fit_nb("cfs_score", ADJ, data = filter(d, cfs_score < 7), label = "adjusted; excluding CFS 7 (n=2)"),
  fit_nb("cfs_score", ADJ, data = filter(d, los_days < max(d$los_days)), label = "adjusted; excluding longest stay (99 d)"),
  fit_nb("cfs_score", c(ADJ, "icans_any", "crs_g2"), label = "adjusted + ICANS + CRS>=2 (toxicity as mediator)"),
  # grip sensitivity: raw kg with sex in the model instead of the normative score
  fit_nb("grip_average_kg", c(ADJ, "sex"), label = "sensitivity: raw grip (kg) + sex + age + log2 LDH")
)
save_table(los_models %>% mutate(across(c(rr, lo, hi), ~ round(.x, 2))), "los_models")
message("\nLength of stay, rate ratio per unit (negative binomial); p < 0.10, plus all CFS models ", SIG_LEGEND, ":")
print(los_models %>% filter(p < 0.1 | predictor == "CFS (per point)") %>%
        transmute(predictor, analysis, rr = fmt_ci(rr, lo, hi), p = fmt_p_star(p), n), n = Inf)

cfs_adj <- los_models %>% filter(predictor == "CFS (per point)", analysis == "adjusted")
record("los_rr_cfs_adj", cfs_adj$rr, lo = cfs_adj$lo, hi = cfs_adj$hi, p = cfs_adj$p, note = "adjusted for age + log2 LDH")
cfs_unadj <- los_models %>% filter(predictor == "CFS (per point)", analysis == "unadjusted")
record("los_rr_cfs_unadj", cfs_unadj$rr, lo = cfs_unadj$lo, hi = cfs_unadj$hi, p = cfs_unadj$p)
cfs_sens <- los_models %>% filter(predictor == "CFS (per point)", str_detect(analysis, "CFS 7"))
record("los_rr_cfs_adj_excl_cfs7", cfs_sens$rr, lo = cfs_sens$lo, hi = cfs_sens$hi, p = cfs_sens$p)

# Prolonged stay by CFS group
los_by_cfs <- d %>%
  group_by(cfs_cat) %>%
  summarise(n = n(), los_median = median(los_days), los_q1 = quantile(los_days, 0.25),
            los_q3 = quantile(los_days, 0.75), prolonged = sum(los_prolonged),
            prolonged_pct = pct(mean(los_prolonged), 1), .groups = "drop")
print(los_by_cfs)
save_table(los_by_cfs, "los_by_cfs")
record("los_prolonged_pct_cfs_gt3", los_by_cfs$prolonged_pct[los_by_cfs$cfs_cat == ">3"])
record("los_prolonged_pct_cfs_1_3", los_by_cfs$prolonged_pct[los_by_cfs$cfs_cat == "1-3"])

lg <- glm(los_prolonged ~ cfs_score + age + log2_ldh, data = d, family = binomial)
or <- exp(cbind(coef(lg), confint.default(lg)))["cfs_score", ]
record("los_prolonged_or_cfs_adj", or[1], lo = or[2], hi = or[3], p = coef(summary(lg))["cfs_score", 4])
record("los_prolonged_auc_cfs", auc_mw(d$cfs_score, d$los_prolonged), note = "CFS alone")
message(sprintf("Prolonged stay: OR per CFS point (adj.) %s; AUC of CFS alone %.2f",
                fmt_ci(or[1], or[2], or[3]), auc_mw(d$cfs_score, d$los_prolonged)))

message("07_toxicity_los.R complete.")
