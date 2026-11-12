# ==============================================================================
# 06_trajectories.R
#
# Purpose : Change of each frailty / quality-of-life measure over the first
#           year. Model per measure (as in the statistician's SAS analysis):
#           mixed model for repeated measures with visit as a categorical
#           fixed effect and a heterogeneous compound-symmetry covariance
#           (nlme::gls, corCompSymm + varIdent by visit), fitted by REML.
#           Reports model-estimated means (LS-means) with 95% CI at each
#           visit and the overall F-test for visit.
#           NOTE: uses all available observations. Patients who die or drop
#           out stop contributing, so later visits over-represent fitter
#           survivors (the model assumes missing-at-random).
# Inputs  : data/processed/v2/analysis_long.rds
# Outputs : output/v2/tables/trajectory_lsmeans.csv, trajectory_raw_means.csv
#           data/processed/v2/trajectory_lsmeans.rds (used by 10_figures.R)
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "06_trajectories.R"
section("06 - Frailty and quality-of-life trajectories over the first year")

al <- read_proc("analysis_long") %>% mutate(study_id = factor(study_id))

outcomes <- c(
  cfs_score = "CFS", walk_mpers = "Gait speed (m/s)", walk_time = "4 m walk time (s)",
  grip_average_kg = "Grip strength (kg)", minicog_score = "Mini-Cog", phq_total_score = "PHQ-9",
  esas_wellbeing = "ESAS well-being", eortc_globalhealth = "EORTC global health",
  eortc_physicalfunction = "EORTC physical function", eortc_rolefunction = "EORTC role function",
  eortc_socialfunction = "EORTC social function", eortc_fatigue = "EORTC fatigue", eq5d_vas = "EQ-5D VAS"
)

## ---- Raw means by visit (descriptive) -------------------------------------------------

raw_means <- map_dfr(names(outcomes), function(v) {
  al %>% group_by(visit) %>%
    summarise(n = sum(!is.na(.data[[v]])), mean = mean(.data[[v]], na.rm = TRUE),
              sd = sd(.data[[v]], na.rm = TRUE), median = median(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
    mutate(var = v, outcome = outcomes[v], .before = 1)
})
save_table(raw_means %>% mutate(across(c(mean, sd), ~ round(.x, 2))), "trajectory_raw_means")

## ---- Mixed model for repeated measures ---------------------------------------------------

fit_mmrm <- function(v) {
  dd <- al %>% filter(!is.na(.data[[v]])) %>% transmute(study_id, visit = droplevels(visit), y = .data[[v]])
  fit <- tryCatch(
    nlme::gls(y ~ 0 + visit, data = dd, method = "REML", na.action = na.omit,
              correlation = nlme::corCompSymm(form = ~ 1 | study_id),
              weights = nlme::varIdent(form = ~ 1 | visit)),
    error = function(e) {
      message("  ", v, ": heterogeneous CS did not converge, using homogeneous CS (", conditionMessage(e), ")")
      nlme::gls(y ~ 0 + visit, data = dd, method = "REML",
                correlation = nlme::corCompSymm(form = ~ 1 | study_id))
    })
  # overall visit effect: same model with an intercept, F-test on the visit term
  fit_int <- update(fit, y ~ visit)
  p_overall <- anova(fit_int)["visit", "p-value"]
  est <- coef(fit); se <- sqrt(diag(vcov(fit)))
  df <- nrow(dd) - length(est)
  tibble(var = v, outcome = outcomes[v], visit = str_remove(names(est), "^visit"),
         n = as.integer(table(dd$visit)[str_remove(names(est), "^visit")]),
         lsmean = est, lo = est - qt(0.975, df) * se, hi = est + qt(0.975, df) * se,
         p_overall = p_overall)
}

lsmeans <- map_dfr(names(outcomes), fit_mmrm) %>%
  mutate(visit = factor(visit, levels = VISIT_LABELS))

lsmeans_wide <- lsmeans %>%
  transmute(outcome, visit, cell = sprintf("%.2f", lsmean), p_overall = fmt_p(p_overall)) %>%
  pivot_wider(names_from = visit, values_from = cell)
message("Model-estimated means by visit (heterogeneous compound symmetry, REML):")
print(lsmeans_wide, n = Inf, width = Inf)

save_table(lsmeans %>% mutate(across(c(lsmean, lo, hi), ~ round(.x, 3))), "trajectory_lsmeans")
save_proc(lsmeans, "trajectory_lsmeans")

cfs <- lsmeans %>% filter(var == "cfs_score")
for (v in VISIT_LABELS) {
  r <- cfs %>% filter(visit == v)
  record(paste0("cfs_lsmean_", str_replace_all(str_to_lower(v), " ", "")), r$lsmean, lo = r$lo, hi = r$hi)
}
record("cfs_trajectory_p", cfs$p_overall[1], p = cfs$p_overall[1])

# Sensitivity: the same model restricted to patients who completed the 12-month
# visit (removes survivor selection from the shape of the curve).
completers <- al %>% filter(visit_month == 12, !is.na(cfs_score)) %>% pull(study_id)
al_c <- al %>% filter(study_id %in% completers)
cfs_completers <- al_c %>% group_by(visit) %>%
  summarise(n = sum(!is.na(cfs_score)), mean = mean(cfs_score, na.rm = TRUE), .groups = "drop")
message("\nCFS mean by visit among the ", length(completers), " patients assessed at 12 months:")
print(cfs_completers)
save_table(cfs_completers, "trajectory_cfs_12m_completers")

message("06_trajectories.R complete.")
