# ==============================================================================
# 08_survival_baseline_models.R
#
# Purpose : Baseline-data Cox proportional-hazards models for overall
#           survival (OS) and progression-free survival (PFS): a
#           comprehensive UVA screen, followed by two MVA models per
#           outcome -
#             (1) a PRIMARY, data-driven model: UVA-significant (p<0.05)
#                 predictors, capped by an events-per-variable (EPV) rule
#                 so the model isn't overfit relative to the number of
#                 events actually observed;
#             (2) a SECONDARY, pre-specified "frailty panel" model (CFS +
#                 Mini-Cog + VES-13), applied identically to both
#                 outcomes, in the same spirit as the prior report's own
#                 baseline MVA models - which combined these instruments
#                 by clinical rationale rather than UVA selection (e.g.
#                 Mini-Cog was not UVA-significant there either, p=0.21,
#                 and still isn't here) - though not a literal
#                 reproduction of either of its two specific models,
#                 which used different variable sets for OS (CFS+Mini-Cog)
#                 vs. PFS (VES13 category+Mini-Cog+CFS).
# Inputs  : data/processed/analysis_wide.rds
# Outputs : output/tables/uva_baseline_os.csv
#           output/tables/uva_baseline_pfs.csv
#           output/tables/mva_baseline_primary.csv
#           output/tables/mva_baseline_frailty_panel.csv
#           output/figures/km_os_by_cfs_cat.png
#           output/figures/km_os_by_ecog_cat.png
# Depends : 00_packages.R, ..., 05_merge_analysis_datasets.R
#
# On the events-per-variable (EPV) cap
# -------------------------------------------------
# With matured follow-up, more baseline covariates now reach UVA
# significance than events can safely support in one model (e.g. 7
# distinct significant constructs for OS against 34 events). Rather than
# fit an uninterpretable 7-predictor model on 34 events, MVA candidates are
# ranked by UVA p-value and capped at floor(n_events / 10) - the standard
# "at least 10 events per predictor" rule of thumb (Peduzzi et al., J Clin
# Epidemiol 1996) for stable Cox regression coefficients.
# ==============================================================================

source(here::here("scripts", "00_packages.R"))

analysis_wide <- readRDS(here("data", "processed", "analysis_wide.rds"))
tables_dir <- here("output", "tables")
figures_dir <- here("output", "figures")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

## ---- Analysis-specific derived categories --------------------------------
# Same clinically-established dichotomisations as 07_toxicity_models.R,
# re-derived here rather than shared, so this script is readable on its own
# (see that script's header for the same design note).

d <- analysis_wide %>%
  mutate(
    ecog_cat = factor(
      case_when(ecog %in% c(0, 1) ~ "0-1", ecog %in% c(2, 3) ~ "2-3", TRUE ~ NA_character_),
      levels = c("0-1", "2-3")
    ),
    ves13_cat = factor(if_else(ves13_score < 3, "<3", ">=3"), levels = c("<3", ">=3")),
    cfs_cat = factor(if_else(cfs_score <= 3, "1-3", ">3"), levels = c("1-3", ">3")),
    walk_mpers_cat = factor(if_else(walk_mpers <= 0.80, "<=0.80", ">0.80"), levels = c("<=0.80", ">0.80"))
  )

## ---- UVA covariate panel --------------------------------------------------

uva_covariates <- c(
  "sex", "bridge_yn", "ecog", "ecog_cat", "age", "ldh", "crp", "albumin",
  "hctci_score", "cirs_total", "ves13_score", "ves13_cat",
  "cfs_score", "cfs_cat", "grip_average_kg", "walk_time", "walk_mpers",
  "walk_mpers_cat", "minicog_score", "phq_total_score"
)

## ---- UVA fitting helper ---------------------------------------------------

#' Fit a single-predictor Cox model and return a tidy summary row per
#' coefficient, including the model's overall likelihood-ratio p-value
#' (valid as "this predictor's overall significance" here specifically
#' because a UVA model has exactly one predictor).
fit_uva_cox <- function(data, time_var, event_var, covariate) {
  formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", covariate)
  fit <- coxph(as.formula(formula_str), data = data)
  broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      covariate = covariate,
      n = fit$n,
      n_event = fit$nevent,
      overall_p = summary(fit)$logtest["pvalue"]
    ) %>%
    relocate(covariate)
}

uva_os <- map_dfr(uva_covariates, ~ fit_uva_cox(d, "os_months", "os_event", .x))
uva_pfs <- map_dfr(uva_covariates, ~ fit_uva_cox(d, "pfs_months", "pfs_event", .x))

message("--- OS UVA: covariates with overall p < 0.10 ---")
uva_os %>% distinct(covariate, overall_p, n, n_event) %>% filter(overall_p < 0.10) %>% arrange(overall_p) %>% print(n = Inf)

message("\n--- PFS UVA: covariates with overall p < 0.10 ---")
uva_pfs %>% distinct(covariate, overall_p, n, n_event) %>% filter(overall_p < 0.10) %>% arrange(overall_p) %>% print(n = Inf)

## ---- MVA candidate selection (EPV-capped) --------------------------------
# Same construct-collapsing idea as 07_toxicity_models.R: a continuous
# score and its established dichotomisation count as one construct, and
# the continuous form is preferred when either reached significance.

construct_of <- c(
  ecog_cat = "ecog", ves13_cat = "ves13", cfs_cat = "cfs",
  walk_time = "walk", walk_mpers = "walk", walk_mpers_cat = "walk"
)
preferred_form <- c(ecog = "ecog", ves13 = "ves13_score", cfs = "cfs_score", walk = "walk_mpers")

#' Rank distinct significant (p < p_threshold) clinical constructs by UVA
#' p-value (most significant first) and cap the count at
#' floor(n_events / epv), returning each construct's preferred
#' representation.
select_mva_candidates_cox <- function(uva_table, n_events, p_threshold = 0.05, epv = 10) {
  ranked <- uva_table %>%
    distinct(covariate, overall_p) %>%
    filter(overall_p < p_threshold) %>%
    mutate(construct = coalesce(unname(construct_of[covariate]), covariate)) %>%
    arrange(overall_p) %>%
    distinct(construct, .keep_all = TRUE)   # first (= most significant) row per construct

  cap <- floor(n_events / epv)
  chosen <- head(ranked$construct, cap)
  unname(coalesce(preferred_form[chosen], chosen))
}

fit_mva_cox <- function(data, time_var, event_var, covariates, label) {
  formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", paste(covariates, collapse = " + "))
  fit <- coxph(as.formula(formula_str), data = data)

  message("\n--- ", label, ": ", formula_str, " (N=", fit$n, ", events=", fit$nevent, ") ---")
  print(broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE))

  ph_test <- cox.zph(fit)
  message("Proportional-hazards check (p < 0.05 flags a violation):")
  print(round(ph_test$table, 3))

  broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(model = label, n = fit$n, n_event = fit$nevent) %>%
    relocate(model)
}

## ---- Primary (data-driven, EPV-capped) MVA --------------------------------

os_n_events <- sum(d$os_event)
pfs_n_events <- sum(d$pfs_event)

os_candidates <- select_mva_candidates_cox(uva_os, os_n_events)
pfs_candidates <- select_mva_candidates_cox(uva_pfs, pfs_n_events)

message("\nOS primary MVA candidates (EPV cap = ", floor(os_n_events / 10), "): ", paste(os_candidates, collapse = ", "))
message("PFS primary MVA candidates (EPV cap = ", floor(pfs_n_events / 10), "): ", paste(pfs_candidates, collapse = ", "))

mva_primary <- bind_rows(
  if (length(os_candidates) >= 2) fit_mva_cox(d, "os_months", "os_event", os_candidates, "OS primary"),
  if (length(pfs_candidates) >= 2) fit_mva_cox(d, "pfs_months", "pfs_event", pfs_candidates, "PFS primary")
)

## ---- Secondary: pre-specified frailty-panel MVA (continuity check) -------
# CFS + Mini-Cog + VES-13, mirroring the prior report's OS Model 4 /
# PFS Model 1 combination. Fit regardless of individual UVA significance -
# this is a pre-specified comparison, not a data-driven selection.

frailty_panel <- c("cfs_score", "minicog_score", "ves13_score")

mva_frailty_panel <- bind_rows(
  fit_mva_cox(d, "os_months", "os_event", frailty_panel, "OS frailty panel (CFS+MiniCog+VES13)"),
  fit_mva_cox(d, "pfs_months", "pfs_event", frailty_panel, "PFS frailty panel (CFS+MiniCog+VES13)")
)

## ---- KM plots for the two most consistently significant predictors -------
# CFS and ECOG are significant for OS both here and in the prior report,
# and are the two constructs most directly relevant to bedside triage.

km_os_cfs <- survfit(Surv(os_months, os_event) ~ cfs_cat, data = d)
p1 <- ggsurvplot(
  km_os_cfs, data = d, risk.table = TRUE, pval = TRUE,
  xlab = "Months from CAR-T infusion", ylab = "Overall survival",
  legend.title = "Baseline CFS", palette = c("#2C7BB6", "#D7191C")
)
png(file.path(figures_dir, "km_os_by_cfs_cat.png"), width = 7, height = 6.5, units = "in", res = 300)
print(p1)
dev.off()

km_os_ecog <- survfit(Surv(os_months, os_event) ~ ecog_cat, data = d)
p2 <- ggsurvplot(
  km_os_ecog, data = d, risk.table = TRUE, pval = TRUE,
  xlab = "Months from CAR-T infusion", ylab = "Overall survival",
  legend.title = "Baseline ECOG", palette = c("#2C7BB6", "#D7191C")
)
png(file.path(figures_dir, "km_os_by_ecog_cat.png"), width = 7, height = 6.5, units = "in", res = 300)
print(p2)
dev.off()

## ---- Save ---------------------------------------------------------------

write_csv(uva_os, file.path(tables_dir, "uva_baseline_os.csv"))
write_csv(uva_pfs, file.path(tables_dir, "uva_baseline_pfs.csv"))
write_csv(mva_primary, file.path(tables_dir, "mva_baseline_primary.csv"))
write_csv(mva_frailty_panel, file.path(tables_dir, "mva_baseline_frailty_panel.csv"))

message("\n08_survival_baseline_models.R complete. Wrote 4 CSVs to output/tables/ and 2 KM plots to output/figures/")
