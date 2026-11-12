# ==============================================================================
# 08_survival_baseline.R
#
# Purpose : Baseline frailty and survival.
#   A. Univariable Cox models for OS and PFS (same panel as the statistician's
#      report, so results can be cross-checked).
#   B. Pre-specified adjusted model: each frailty measure + age + log2 LDH.
#      (No p-value-driven variable selection: 34 deaths support ~3 terms.)
#   C. Competing risks: relapse-related (RM) vs non-relapse mortality (NRM),
#      cause-specific Cox and Fine-Gray, unadjusted and adjusted; sensitivity
#      with the standard definition (death without vs after progression).
#   D. Time-varying CFS (updated at each visit) for OS and PFS - exploratory:
#      likely partly reverse causation (frailty rising as death approaches).
# Inputs  : data/processed/v2/analysis_wide.rds, analysis_long.rds
# Outputs : output/v2/tables/cox_univariable.csv, cox_adjusted.csv,
#           competing_risks.csv, cox_time_varying.csv
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "08_survival_baseline.R"
section("08 - Baseline frailty and survival")

d  <- read_proc("analysis_wide")
al <- read_proc("analysis_long")

## ---- A. Univariable Cox -------------------------------------------------------------------

uva_panel <- c("sex", "bridge_yn", "ecog_cat", "age", "ldh", "log2_ldh", "crp", "albumin", "hctci_score",
               "cirs_total", "ves13_score", "ves_cat", "cfs_score", "cfs_cat", "cfs_cat4",
               "grip_average_kg", "walk_time", "walk_mpers", "gait_cat", "gait_slow_nd",
               "minicog_score", "phq_total_score")

uva <- bind_rows(
  map_dfr(uva_panel, ~ cox_fit(paste("Surv(os_months, os_event) ~", .x), d) %>% mutate(outcome = "OS", covariate = .x)),
  map_dfr(uva_panel, ~ cox_fit(paste("Surv(pfs_months, pfs_event) ~", .x), d) %>% mutate(outcome = "PFS", covariate = .x))
) %>% relocate(outcome, covariate)
save_table(uva %>% mutate(across(c(est, lo, hi, c_index), ~ round(.x, 3))), "cox_univariable")

message("Univariable Cox, p < 0.10:")
print(uva %>% filter(p < 0.10) %>% transmute(outcome, term, hr = fmt_ci(est, lo, hi), p = fmt_p(p), n, events), n = Inf)

get_uva <- function(outc, term) uva %>% filter(outcome == outc, .data$term == !!term)
r <- get_uva("OS", "cfs_score");   record("os_hr_cfs_unadj", r$est, r$lo, r$hi, r$p)
r <- get_uva("PFS", "cfs_score");  record("pfs_hr_cfs_unadj", r$est, r$lo, r$hi, r$p)
r <- get_uva("OS", "ecog_cat2-3"); record("os_hr_ecog23", r$est, r$lo, r$hi, r$p, note = paste("n =", r$n))
r <- get_uva("OS", "gait_slow_ndSlow or not done"); record("os_hr_gait_slow_or_nd", r$est, r$lo, r$hi, r$p)
r <- get_uva("OS", "minicog_score"); record("os_hr_minicog", r$est, r$lo, r$hi, r$p)
r <- get_uva("OS", "phq_total_score"); record("os_hr_phq", r$est, r$lo, r$hi, r$p)

## ---- B. Pre-specified adjusted models: measure + age + log2 LDH ---------------------------

adj_measures <- c("cfs_score", "ves13_score", "ves_cat", "walk_mpers", "gait_slow_nd", "grip_average_kg",
                  "minicog_score", "phq_total_score", "hctci_score", "cirs_total")
adjusted <- bind_rows(
  map_dfr(adj_measures, ~ cox_fit(paste("Surv(os_months, os_event) ~", .x, "+ age + log2_ldh"), d) %>%
            mutate(outcome = "OS", covariate = .x)),
  map_dfr(adj_measures, ~ cox_fit(paste("Surv(pfs_months, pfs_event) ~", .x, "+ age + log2_ldh"), d) %>%
            mutate(outcome = "PFS", covariate = .x))
) %>% filter(!term %in% c("age", "log2_ldh")) %>% relocate(outcome, covariate)
save_table(adjusted %>% mutate(across(c(est, lo, hi, c_index), ~ round(.x, 3))), "cox_adjusted")
message("\nFrailty measures adjusted for age + log2 LDH:")
print(adjusted %>% transmute(outcome, term, hr = fmt_ci(est, lo, hi), p = fmt_p(p), n, events), n = Inf)

r <- adjusted %>% filter(outcome == "OS", term == "cfs_score");  record("os_hr_cfs_adj", r$est, r$lo, r$hi, r$p)
r <- adjusted %>% filter(outcome == "PFS", term == "cfs_score"); record("pfs_hr_cfs_adj", r$est, r$lo, r$hi, r$p)
r <- adjusted %>% filter(outcome == "OS", term == "gait_slow_ndSlow or not done")
record("os_hr_gait_slow_or_nd_adj", r$est, r$lo, r$hi, r$p)

# Proportional hazards check for the main adjusted model
zph <- cox.zph(coxph(Surv(os_months, os_event) ~ cfs_score + age + log2_ldh, data = d))
message("\nPH test, OS ~ CFS + age + log2 LDH:"); print(round(zph$table, 3))

## ---- C. Competing risks: RM vs NRM --------------------------------------------------------

message(sprintf("\nCause of death: RM %d, NRM %d, unknown %d | standard definition: after progression %d, without %d",
                sum(d$cr_cause == 1), sum(d$cr_cause == 2), sum(d$cr_cause == 3),
                sum(d$cr_prog == 1), sum(d$cr_prog == 2)))

cr_specs <- expand_grid(
  definition = c("cause", "prog"),
  covs = list(c("cfs_score"), c("cfs_score", ADJ), c("ves13_score"), c("ves13_score", ADJ),
              c("gait_slow_nd"), c("gait_slow_nd", ADJ)),
  failcode = 1:2
)

cs_cox <- function(data, status, failcode, covs) {
  dd <- data %>% mutate(ev = as.integer(.data[[status]] == failcode))
  cox_fit(paste("Surv(os_months, ev) ~", paste(covs, collapse = " + ")), dd)[1, ]
}

competing <- pmap_dfr(cr_specs, function(definition, covs, failcode) {
  status <- if (definition == "cause") "cr_cause" else "cr_prog"
  cause_lab <- if (definition == "cause") c("RM", "NRM")[failcode] else c("Death after progression", "Death without progression")[failcode]
  fg <- fg_fit(d, "os_months", status, covs, failcode)[1, ] %>% mutate(method = "Fine-Gray (sHR)")
  cs <- cs_cox(d, status, failcode, covs) %>% mutate(method = "Cause-specific Cox (HR)")
  bind_rows(fg %>% select(term, est, lo, hi, p, n, events, method),
            cs %>% select(term, est, lo, hi, p, n, events, method)) %>%
    mutate(definition = if (definition == "cause") "Cause-of-death attribution (primary)" else "Death with vs without prior progression",
           cause = cause_lab, adjusted = if (length(covs) > 1) "age + log2 LDH" else "none", .before = 1)
})
save_table(competing %>% mutate(across(c(est, lo, hi), ~ round(.x, 3))), "competing_risks")
message("\nCompeting risks (first term shown):")
print(competing %>% transmute(definition = str_sub(definition, 1, 12), cause, method = str_sub(method, 1, 14),
                              term, adjusted, est = fmt_ci(est, lo, hi), p = fmt_p(p), events), n = Inf)

pick <- function(def, cause, meth, term, adj) competing %>%
  filter(str_detect(definition, def), .data$cause == !!cause, str_detect(method, meth),
         .data$term == !!term, adjusted == adj)
r <- pick("primary", "RM", "Fine-Gray", "cfs_score", "none");            record("rm_shr_cfs_unadj", r$est, r$lo, r$hi, r$p)
r <- pick("primary", "NRM", "Fine-Gray", "cfs_score", "none");           record("nrm_shr_cfs_unadj", r$est, r$lo, r$hi, r$p)
r <- pick("primary", "RM", "Fine-Gray", "cfs_score", "age + log2 LDH");  record("rm_shr_cfs_adj", r$est, r$lo, r$hi, r$p)
r <- pick("primary", "NRM", "Fine-Gray", "cfs_score", "age + log2 LDH"); record("nrm_shr_cfs_adj", r$est, r$lo, r$hi, r$p)

## ---- D. Time-varying CFS ------------------------------------------------------------------
# Counting-process data built with tmerge(); the CFS value is carried forward
# from each visit until the next one. Nominal visit months are used (actual
# visit dates give the same estimates; see the v1 validation).
# Lagged sensitivity: each CFS value only starts to apply at the NEXT visit,
# which reduces (does not remove) the reverse-causation problem.

build_cp <- function(time, event, lag = FALSE) {
  base <- d %>% transmute(study_id, tt = .data[[time]], ee = .data[[event]], age, log2_ldh)
  vis <- al %>% filter(!is.na(cfs_score)) %>% select(study_id, visit_month, cfs_score) %>% arrange(study_id, visit_month)
  if (lag) {
    vis <- vis %>% group_by(study_id) %>%
      mutate(visit_month = lead(visit_month)) %>% ungroup() %>% filter(!is.na(visit_month))
  }
  cp <- tmerge(base, base, id = study_id, event = event(tt, ee))
  cp <- tmerge(cp, vis, id = study_id, cfs = tdc(visit_month, cfs_score))
  cp
}

tv <- bind_rows(
  map_dfr(c(FALSE, TRUE), function(lg) {
    bind_rows(
      cox_fit("Surv(tstart, tstop, event) ~ cfs", build_cp("os_months", "os_event", lg)) %>% mutate(outcome = "OS", adjusted = "none"),
      cox_fit("Surv(tstart, tstop, event) ~ cfs + age + log2_ldh", build_cp("os_months", "os_event", lg)) %>% mutate(outcome = "OS", adjusted = "age + log2 LDH"),
      cox_fit("Surv(tstart, tstop, event) ~ cfs", build_cp("pfs_months", "pfs_event", lg)) %>% mutate(outcome = "PFS", adjusted = "none"),
      cox_fit("Surv(tstart, tstop, event) ~ cfs + age + log2_ldh", build_cp("pfs_months", "pfs_event", lg)) %>% mutate(outcome = "PFS", adjusted = "age + log2 LDH")
    ) %>% mutate(cfs_value = if (lg) "lagged (previous visit)" else "current (last visit)")
  })
) %>% filter(term == "cfs") %>% relocate(outcome, cfs_value, adjusted)
save_table(tv %>% mutate(across(c(est, lo, hi, c_index), ~ round(.x, 3))), "cox_time_varying")
message("\nTime-varying CFS (HR per point):")
print(tv %>% transmute(outcome, cfs_value, adjusted, hr = fmt_ci(est, lo, hi), p = fmt_p(p), events))

r <- tv %>% filter(outcome == "OS", adjusted != "none", str_detect(cfs_value, "current"))
record("tv_os_hr_cfs_adj", r$est, r$lo, r$hi, r$p)
r <- tv %>% filter(outcome == "OS", adjusted == "none", str_detect(cfs_value, "current"))
record("tv_os_hr_cfs_unadj", r$est, r$lo, r$hi, r$p)
r <- tv %>% filter(outcome == "OS", adjusted != "none", str_detect(cfs_value, "lagged"))
record("tv_os_hr_cfs_adj_lagged", r$est, r$lo, r$hi, r$p)

# How close to death was the last CFS assessment?
last_cfs <- al %>% filter(!is.na(cfs_score)) %>% group_by(study_id) %>% slice_max(visit_month, n = 1) %>% ungroup() %>%
  left_join(d %>% select(study_id, os_months, os_event), by = c("study_id", "os_months", "os_event"))
gap <- with(last_cfs[last_cfs$os_event == 1, ], os_months - actual_month)
record("months_last_cfs_to_death_median", median(gap, na.rm = TRUE), note = "deaths only")

message("08_survival_baseline.R complete.")
