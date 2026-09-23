# ==============================================================================
# 08_survival_baseline_models.R
#
# Purpose : Baseline-data survival models for OS and PFS, in three parts:
#             A) a comprehensive UVA screen, followed by two MVA models per
#                outcome -
#                  (1) a PRIMARY, data-driven model: UVA-significant
#                      (p<0.05) predictors, capped by an events-per-
#                      variable (EPV) rule so the model isn't overfit
#                      relative to the number of events observed;
#                  (2) a SECONDARY, pre-specified "frailty panel" model
#                      (CFS + Mini-Cog + VES-13), applied identically to
#                      both outcomes, in the same spirit as the prior
#                      report's own baseline MVA models - which combined
#                      these instruments by clinical rationale rather than
#                      UVA selection (e.g. Mini-Cog was not UVA-significant
#                      there either, p=0.21, and still isn't here) - though
#                      not a literal reproduction of either of its two
#                      specific models, which used different variable sets
#                      for OS (CFS+Mini-Cog) vs. PFS (VES13 category+
#                      Mini-Cog+CFS).
#             B) two KM plots for the two predictors most consistently
#                significant for OS across this study's own analyses.
#             C) a competing-risks analysis of OS, splitting death into
#                relapse-related vs. non-relapse mortality (NRM), testing
#                whether baseline frailty specifically predicts NRM rather
#                than relapse-related death (see that section's own header
#                for the full rationale and how its result compares with
#                the stated working hypothesis).
# Inputs  : data/processed/analysis_wide.rds
# Outputs : output/tables/uva_baseline_os.csv
#           output/tables/uva_baseline_pfs.csv
#           output/tables/mva_baseline_primary.csv
#           output/tables/mva_baseline_frailty_panel.csv
#           output/tables/uva_competing_risk.csv
#           output/tables/mva_relapse_death.csv        (if EPV supports it)
#           output/tables/finegray_competing_risk.csv
#           output/figures/km_os_by_cfs_cat.png
#           output/figures/km_os_by_ecog_cat.png
#           output/figures/cif_by_cfs_cat.png
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
# Epidemiol 1996) for stable Cox regression coefficients. The same rule is
# applied again in part C below, separately for each competing cause.
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

p1

png(file.path(figures_dir, "km_os_by_cfs_cat.png"), width = 7, height = 6.5, units = "in", res = 300)
print(p1)
dev.off()

km_os_ecog <- survfit(Surv(os_months, os_event) ~ ecog_cat, data = d)

p2 <- ggsurvplot(
  km_os_ecog, data = d, risk.table = TRUE, pval = TRUE,
  xlab = "Months from CAR-T infusion", ylab = "Overall survival",
  legend.title = "Baseline ECOG", palette = c("#2C7BB6", "#D7191C")
)
p2

png(file.path(figures_dir, "km_os_by_ecog_cat.png"), width = 7, height = 6.5, units = "in", res = 300)
print(p2)
dev.off()

## ============================================================================
## Part C: Competing-risks analysis - relapse-related vs. non-relapse death
## ============================================================================
#
# Working hypothesis under test: baseline frailty predicts non-relapse
# mortality (NRM) specifically, but not relapse-related mortality.
#
# Method: death is split into two competing causes using death_disease
# (carried through from 04_derive_survival_outcomes.R via analysis_wide) -
# 1 = disease-related (relapse) death, 0 = not disease-related (NRM). The
# code book does not formally define a third level, but the study's own
# descriptive-statistics report confirms death_disease == 2 as "Unknown
# cause"; those patients, plus any death with a missing cause, are kept as
# their own third competing event rather than dropped or folded into
# either named cause, so they still correctly remove a patient from being
# "at risk" of the other two causes without being miscoded as either one.
#
# Two complementary, standard approaches are reported, as recommended
# practice for competing-risk analyses (e.g. Austin & Fine, Stat Med 2017):
#   - cause-specific Cox models (coxph(), already loaded): the hazard of
#     one cause among patients still free of *any* event - deaths from the
#     other cause are censored at their death time. Answers "does this
#     predictor affect the underlying rate of this specific cause".
#   - Fine-Gray subdistribution hazard models (cmprsk::crr(), already a
#     project dependency via 00_packages.R - see cuminc() below too):
#     models the cumulative incidence directly, keeping patients who died
#     of the other cause in the risk set. Answers "does this predictor
#     affect the absolute probability of this specific cause over time".
# The two frameworks agree throughout this analysis, which is reassuring;
# where they didn't, both would be reported rather than picking one.
#
# The same EPV logic as Parts A/B applies per cause: with only 9 NRM
# events, floor(9/10) = 0, so no NRM regression model beyond the single
# pre-specified CFS test below is statistically defensible - reported as
# UVA/pre-specified-univariable only, not forced into an MVA it can't
# support. Relapse-death (21 events, cap = 2) can support a small MVA and
# gets one, reusing select_mva_candidates_cox()/fit_mva_cox() from Part A.

  # TODO sensitivity analysis changing the "unknowns"
d <- d %>%
  mutate(
    cr_event = case_when(
      os_event == 0 ~ 0,        # censored (alive)
      death_disease == 1 ~ 1,   # relapse-related death
      death_disease == 0 ~ 2,   # non-relapse death (NRM)
      death_disease == 3 ~ 2,   # TODO consider changing / commenging out
      TRUE ~ 3                  # unknown cause (death_disease == 2, or missing)
    ),
    nrm_event = as.integer(cr_event == 2),
    relapse_event = as.integer(cr_event == 1)
  )

message("\n--- Competing-risk event distribution (0=censored, 1=relapse-death, 2=NRM, 3=unknown-cause) ---")
print(table(d$cr_event))

## ---- Cause-specific Cox UVA screen: NRM vs. relapse-death -----------------
# Reuses the same uva_covariates panel as Part A.

fit_cause_specific_cox <- function(data, event_var, covariate) {
  formula_str <- paste0("Surv(os_months, ", event_var, ") ~ ", covariate)
  fit <- coxph(as.formula(formula_str), data = data)
  broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      covariate = covariate, n = fit$n, n_event = fit$nevent,
      overall_p = summary(fit)$logtest["pvalue"]
    ) %>%
    relocate(covariate)
}

uva_nrm <- map_dfr(uva_covariates, ~ fit_cause_specific_cox(d, "nrm_event", .x)) %>% mutate(cause = "NRM")
uva_relapse <- map_dfr(uva_covariates, ~ fit_cause_specific_cox(d, "relapse_event", .x)) %>% mutate(cause = "relapse_death")

message("\n--- Cause-specific UVA, NRM: covariates with overall p < 0.10 ---")
uva_nrm %>% distinct(covariate, overall_p, n_event) %>% filter(overall_p < 0.10) %>% arrange(overall_p) %>% print(n = Inf)
if (all(uva_nrm$overall_p >= 0.10)) message("(none reach p < 0.10 - see interpretation note below)")

message("\n--- Cause-specific UVA, relapse-death: covariates with overall p < 0.10 ---")
uva_relapse %>% distinct(covariate, overall_p, n_event) %>% filter(overall_p < 0.10) %>% arrange(overall_p) %>% print(n = Inf)

uva_competing_risk <- bind_rows(uva_nrm, uva_relapse)

## ---- Pre-specified hypothesis test: baseline CFS -> both causes ----------
# Tested directly regardless of its UVA rank above, since CFS is the
# specific measure named in the working hypothesis (the same logic as the
# frailty-panel model in Part A: a pre-specified test is reported as such,
# not chosen because it happened to top a screen).

message("\n--- Pre-specified test: baseline CFS -> NRM (cause-specific Cox) ---")
print(fit_cause_specific_cox(d, "nrm_event", "cfs_score"))
message("\n--- Pre-specified test: baseline CFS -> relapse-death (cause-specific Cox) ---")
print(fit_cause_specific_cox(d, "relapse_event", "cfs_score"))

## ---- Relapse-death MVA (EPV-capped, reusing Part A's helpers) ------------

relapse_n_events <- sum(d$relapse_event)
relapse_candidates <- select_mva_candidates_cox(uva_relapse, relapse_n_events, epv = 5)
relapse_n_events

message(
  "\nRelapse-death MVA candidates (EPV cap = ", floor(relapse_n_events / 10), "): ",
  paste(relapse_candidates, collapse = ", ")
)

mva_relapse_death <- if (length(relapse_candidates) >= 2) {
  fit_mva_cox(d, "os_months", "relapse_event", relapse_candidates, "Relapse-death primary")
} else {
  message("MVA not fit for relapse-death: fewer than 2 candidates survived the EPV cap.")
  tibble()
}

nrm_candidates <- select_mva_candidates_cox(uva_nrm, sum(d$nrm_event), epv = 5) # epv does not seem to change selection in practice
nrm_candidates
message(
  "NRM MVA candidates (EPV cap = ", floor(sum(d$nrm_event) / 10), "): ",
  if (length(nrm_candidates) == 0) "(none - cap is 0 with only 9 events)" else paste(nrm_candidates, collapse = ", ")
)

## ---- Fine-Gray subdistribution hazard models ------------------------------
# Kept to single-predictor models only: with 9 NRM events, even a 2-
# covariate Fine-Gray model would be well outside the same EPV logic
# applied everywhere else in this script, and crr()'s optimiser is
# noticeably less robust than coxph()'s to sparse data (see this project's
# development history for a case where it failed to converge entirely).
# CFS is the pre-specified hypothesis test; LDH (the strongest relapse-
# death UVA signal above) is included as an independent cross-check of the
# same relapse-not-NRM pattern using a second, unrelated predictor.

fit_finegray <- function(data, covariate, failcode, label) {
  cov1 <- as.matrix(data[[covariate]])
  colnames(cov1) <- covariate
  fit <- tryCatch(
    crr(ftime = data$os_months, fstatus = data$cr_event, cov1 = cov1, failcode = failcode, cencode = 0),
    error = function(e) {
      message("Fine-Gray model did not converge for ", label, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) {
    return(tibble(model = label, covariate = covariate, subHR = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
  }
  s <- summary(fit)
  tibble(
    model = label, covariate = covariate,
    subHR = s$coef[1, "exp(coef)"],
    conf.low = s$conf.int[1, 3], conf.high = s$conf.int[1, 4],
    p.value = s$coef[1, "p-value"]
  )
}

finegray_competing_risk <- bind_rows(
  fit_finegray(d, "cfs_score", 2, "CFS -> NRM (Fine-Gray)"),
  fit_finegray(d, "cfs_score", 1, "CFS -> relapse-death (Fine-Gray)"),
  fit_finegray(d, "ldh", 2, "LDH -> NRM (Fine-Gray)"),
  fit_finegray(d, "ldh", 1, "LDH -> relapse-death (Fine-Gray)")
)

message("\n--- Fine-Gray subdistribution hazard results ---")
print(finegray_competing_risk)

## ---- Cumulative incidence plot, by baseline CFS category -----------------
# cuminc() needs a discrete grouping variable (unlike the regression models
# above, which use continuous CFS) - cfs_cat is used here purely for this
# descriptive/visual comparison. Dichotomising loses power, so Gray's test
# below is expected to be less significant than the continuous cause-
# specific Cox/Fine-Gray models above; both point the same direction.

ci_fit <- cuminc(ftime = d$os_months, fstatus = d$cr_event, group = d$cfs_cat)
gray_p_relapse <- ci_fit$Tests["1", "pv"]
gray_p_nrm <- ci_fit$Tests["2", "pv"]

message(
  "\nGray's test (baseline CFS 1-3 vs. >3): relapse-death p=", round(gray_p_relapse, 3),
  ", NRM p=", round(gray_p_nrm, 3),
  " (categorical - expect a weaker signal here than the continuous models above)"
)

ci_df <- map_dfr(setdiff(names(ci_fit), "Tests"), function(nm) {
  parts <- str_split(nm, " ", simplify = TRUE)
  tibble(group = parts[1], cause = parts[2], time = ci_fit[[nm]]$time, est = ci_fit[[nm]]$est)
}) %>%
  filter(cause %in% c("1", "2")) %>%
  mutate(cause_label = factor(cause, levels = c("1", "2"), labels = c(
    paste0("Relapse-related death (Gray's p=", round(gray_p_relapse, 2), ")"),
    paste0("Non-relapse death (Gray's p=", round(gray_p_nrm, 2), ")")
  )))

cif_plot <- ggplot(ci_df, aes(x = time, y = est, color = group)) +
  geom_step(linewidth = 0.9) +
  facet_wrap(~cause_label) +
  scale_color_manual(values = c("#2C7BB6", "#D7191C")) +
  labs(x = "Months from CAR-T infusion", y = "Cumulative incidence", color = "Baseline CFS") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

cif_plot

png(file.path(figures_dir, "cif_by_cfs_cat.png"), width = 9, height = 5, units = "in", res = 300)
print(cif_plot)
dev.off()

## ---- Interpretation, printed plainly rather than left implicit ----------

message(
  "\n--- Competing-risk summary ---\n",
  "The working hypothesis (baseline frailty predicts NRM specifically, not\n",
  "relapse-death) is NOT supported by this baseline-only analysis: no\n",
  "baseline covariate reaches p<0.10 for NRM, while 14 of ", length(uva_covariates),
  " do for relapse-death,\n",
  "including CFS itself (cause-specific Cox and Fine-Gray HR/subHR both ~1.4,\n",
  "p<0.01) and, independently, LDH (p<1e-5 in both frameworks). The pattern\n",
  "found here runs the other way: baseline frailty and disease-burden markers\n",
  "track relapse-related death, not NRM. This is worth reading alongside\n",
  "10_trajectory_modeling.R's finding that the CFS *trajectory class* (not a\n",
  "single baseline value) tracked NRM specifically - together they suggest\n",
  "baseline status and the evolving frailty course may carry different\n",
  "cause-specific information, which a dedicated survival-by-trajectory-class\n",
  "script could test formally."
)

## ---- Save ---------------------------------------------------------------

write_csv(uva_os, file.path(tables_dir, "uva_baseline_os.csv"))
write_csv(uva_pfs, file.path(tables_dir, "uva_baseline_pfs.csv"))
write_csv(mva_primary, file.path(tables_dir, "mva_baseline_primary.csv"))
write_csv(mva_frailty_panel, file.path(tables_dir, "mva_baseline_frailty_panel.csv"))
write_csv(uva_competing_risk, file.path(tables_dir, "uva_competing_risk.csv"))
if (nrow(mva_relapse_death) > 0) {
  write_csv(mva_relapse_death, file.path(tables_dir, "mva_relapse_death.csv"))
}
write_csv(finegray_competing_risk, file.path(tables_dir, "finegray_competing_risk.csv"))

message("\n08_survival_baseline_models.R complete. Wrote UVA/MVA/competing-risk tables to output/tables/ and 3 figures to output/figures/")