# ==============================================================================
# 09_survival_timevarying_models.R
#
# Purpose : Time-dependent Cox models for OS and PFS, using the repeated
#           frailty assessments as time-varying covariates. Follows the
#           study's own repeated-measures analysis (baseline-collected
#           covariates held constant; CFS, grip strength, Mini-Cog, PHQ-9,
#           and the 4m walk test updated at each visit) via the standard
#           counting-process (start/stop) construction, built with
#           survival::tmerge() rather than by hand.
# Inputs  : data/processed/baseline_clean.rds       (static covariates)
#           data/processed/analysis_survival.rds    (event times/status)
#           data/processed/repeated_measures_clean.rds (time-varying scores)
# Outputs : output/tables/uva_timevarying_os.csv
#           output/tables/uva_timevarying_pfs.csv
#           output/tables/mva_timevarying_primary.csv
# Depends : 00_packages.R, ..., 04_derive_survival_outcomes.R
#
# How the counting-process data is built
# -------------------------------------------------
# Each patient's follow-up is cut into consecutive intervals at every visit
# time and at their final event/censoring time. A time-varying covariate's
# value is carried forward unchanged from one visit until the next
# (standard "last value carried forward" construction) - e.g. a patient
# with visits at 0, 1, 3 months and CFS scores 4, 5, 3, who then dies at
# 4.2 months, contributes three intervals: [0,1) at CFS=4, [1,3) at CFS=5,
# and [3, 4.2] (event) at CFS=3. This was verified by hand against a known
# patient before being applied to the whole cohort - see the project's
# development history if that check is ever needed again.
#
# Static (baseline-only) covariates are attached once, at the initial
# tmerge() call, rather than being re-derived per interval - see the
# `static_covs` block below.
#
# Not using cluster(study_id) robust standard errors: each patient
# contributes exactly one terminal event (death, progression, or
# censoring) spread across several non-overlapping time intervals, not
# multiple recurrent events - the setting where a clustering correction is
# usually needed. The standard model-based Cox standard errors are
# appropriate here.
# ==============================================================================

source(here::here("scripts", "00_packages.R"))

baseline_clean <- readRDS(here("data", "processed", "baseline_clean.rds"))
analysis_survival <- readRDS(here("data", "processed", "analysis_survival.rds"))
repeated_measures_clean <- readRDS(here("data", "processed", "repeated_measures_clean.rds"))

tables_dir <- here("output", "tables")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

## ---- Build the counting-process datasets ---------------------------------

static_covs <- baseline_clean %>%
  select(study_id, age, sex, ldh, crp, albumin, hctci_score, cirs_total, ves13_score, bridge_yn, ecog)

#' Build a counting-process (start/stop) dataset for one time-to-event
#' outcome, with the six repeated-measures scores attached as time-varying
#' covariates (last value carried forward between visits).
build_counting_process <- function(time_var, event_var) {
  # tmerge() is not tidyeval-aware, so its event()/tdc() arguments can't be
  # built with rlang unquoting - the outcome columns are renamed to fixed
  # generic names instead, so the same literal event() call works for both
  # OS and PFS.
  base <- analysis_survival %>%
    select(study_id, time_col = all_of(time_var), event_col = all_of(event_var)) %>%
    inner_join(static_covs, by = "study_id")

  cp <- tmerge(data1 = base, data2 = base, id = study_id, event = event(time_col, event_col))

  cp <- tmerge(cp, repeated_measures_clean, id = study_id, cfs = tdc(visit_month, cfs_score))
  cp <- tmerge(cp, repeated_measures_clean, id = study_id, grip = tdc(visit_month, grip_average_kg))
  cp <- tmerge(cp, repeated_measures_clean, id = study_id, walk_time_tv = tdc(visit_month, walk_time))
  cp <- tmerge(cp, repeated_measures_clean, id = study_id, walk_speed_tv = tdc(visit_month, walk_mpers))
  cp <- tmerge(cp, repeated_measures_clean, id = study_id, minicog = tdc(visit_month, minicog_score))
  cp <- tmerge(cp, repeated_measures_clean, id = study_id, phq_total = tdc(visit_month, phq_total_score))

  cp
}

cp_os <- build_counting_process("os_months", "os_event")
cp_pfs <- build_counting_process("pfs_months", "pfs_event")

stopifnot(
  "OS events not preserved by tmerge" = sum(cp_os$event) == sum(analysis_survival$os_event),
  "PFS events not preserved by tmerge" = sum(cp_pfs$event) == sum(analysis_survival$pfs_event),
  "invalid (tstart >= tstop) interval in cp_os" = all(cp_os$tstart < cp_os$tstop),
  "invalid (tstart >= tstop) interval in cp_pfs" = all(cp_pfs$tstart < cp_pfs$tstop)
)

message("cp_os:  ", nrow(cp_os), " intervals, ", n_distinct(cp_os$study_id), " patients, ", sum(cp_os$event), " events")
message("cp_pfs: ", nrow(cp_pfs), " intervals, ", n_distinct(cp_pfs$study_id), " patients, ", sum(cp_pfs$event), " events")

## ---- UVA covariate panel --------------------------------------------------
# Static (baseline) + time-varying (repeated-measures) covariates, matching
# the study's own repeated-measures analysis panel.

uva_covariates <- c(
  "age", "sex", "ldh", "crp", "albumin", "hctci_score", "cirs_total", "ves13_score",
  "bridge_yn", "ecog",
  "cfs", "grip", "walk_time_tv", "walk_speed_tv", "minicog", "phq_total"
)

fit_uva_cox_cp <- function(cp_data, covariate) {
  formula_str <- paste0("Surv(tstart, tstop, event) ~ ", covariate)
  fit <- coxph(as.formula(formula_str), data = cp_data)

  # computed before the tidy()/mutate() pipe below - inside a mutate(),
  # `data` (or any name bound to the tibble being mutated) refers to the
  # *current* pipeline tibble, not this function's argument, so this must
  # not be inlined into that chain
  n_patients <- n_distinct(cp_data$study_id[!is.na(cp_data[[covariate]])])

  broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      covariate = covariate,
      n_obs = fit$n,
      n_patients = n_patients,
      n_event = fit$nevent,
      overall_p = summary(fit)$logtest["pvalue"]
    ) %>%
    relocate(covariate)
}

uva_os_tv <- map_dfr(uva_covariates, ~ fit_uva_cox_cp(cp_os, .x))
uva_pfs_tv <- map_dfr(uva_covariates, ~ fit_uva_cox_cp(cp_pfs, .x))

message("\n--- OS (time-varying) UVA: covariates with overall p < 0.10 ---")
uva_os_tv %>% distinct(covariate, overall_p, n_obs, n_event) %>% filter(overall_p < 0.10) %>% arrange(overall_p) %>% print(n = Inf)

message("\n--- PFS (time-varying) UVA: covariates with overall p < 0.10 ---")
uva_pfs_tv %>% distinct(covariate, overall_p, n_obs, n_event) %>% filter(overall_p < 0.10) %>% arrange(overall_p) %>% print(n = Inf)

## ---- MVA candidate selection (EPV-capped, same rule as script 08) --------
# No continuous/categorical duplicate forms in this panel (unlike scripts
# 07/08), so no construct-collapsing step is needed here.

select_mva_candidates <- function(uva_table, n_events, p_threshold = 0.05, epv = 10) {
  ranked <- uva_table %>%
    distinct(covariate, overall_p) %>%
    filter(overall_p < p_threshold) %>%
    arrange(overall_p)

  cap <- floor(n_events / epv)
  head(ranked$covariate, cap)
}

fit_mva_cox_cp <- function(data, covariates, label) {
  formula_str <- paste0("Surv(tstart, tstop, event) ~ ", paste(covariates, collapse = " + "))
  fit <- coxph(as.formula(formula_str), data = data)

  message("\n--- ", label, ": ", formula_str, " (obs=", fit$n, ", events=", fit$nevent, ") ---")
  print(broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE))

  ph_test <- cox.zph(fit)
  message("Proportional-hazards check (p < 0.05 flags a violation):")
  print(round(ph_test$table, 3))

  broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(model = label, n_obs = fit$n, n_event = fit$nevent) %>%
    relocate(model)
}

os_candidates <- select_mva_candidates(uva_os_tv, sum(cp_os$event))
pfs_candidates <- select_mva_candidates(uva_pfs_tv, sum(cp_pfs$event))

message("\nOS (time-varying) primary MVA candidates (EPV cap = ", floor(sum(cp_os$event) / 10), "): ", paste(os_candidates, collapse = ", "))
message("PFS (time-varying) primary MVA candidates (EPV cap = ", floor(sum(cp_pfs$event) / 10), "): ", paste(pfs_candidates, collapse = ", "))

mva_timevarying <- bind_rows(
  if (length(os_candidates) >= 2) fit_mva_cox_cp(cp_os, os_candidates, "OS time-varying primary"),
  if (length(pfs_candidates) >= 2) fit_mva_cox_cp(cp_pfs, pfs_candidates, "PFS time-varying primary")
)

## ---- Save ---------------------------------------------------------------

write_csv(uva_os_tv, file.path(tables_dir, "uva_timevarying_os.csv"))
write_csv(uva_pfs_tv, file.path(tables_dir, "uva_timevarying_pfs.csv"))
if (nrow(mva_timevarying) > 0) {
  write_csv(mva_timevarying, file.path(tables_dir, "mva_timevarying_primary.csv"))
}

message("\n09_survival_timevarying_models.R complete.")
