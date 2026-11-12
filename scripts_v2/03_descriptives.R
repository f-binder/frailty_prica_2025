# ==============================================================================
# 03_descriptives.R
#
# Purpose : Cohort description (Table 1), events, causes of death, follow-up,
#           OS / PFS and cumulative incidence of relapse-related (RM) and
#           non-relapse mortality (NRM).
# Inputs  : data/processed/v2/analysis_wide.rds, analysis_long.rds
# Outputs : output/v2/tables/table1_categorical.csv, table1_continuous.csv,
#           events.csv, causes_of_death.csv, survival_estimates.csv
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "03_descriptives.R"
section("03 - Descriptive statistics")

d  <- read_proc("analysis_wide")
al <- read_proc("analysis_long")
N  <- nrow(d)

## ---- Table 1 ----------------------------------------------------------------------

describe_cat <- function(var) {
  x <- d[[var]]
  tab <- tibble(level = as.character(x)) %>% filter(!is.na(level)) %>% count(level)
  # keep factor level order where there is one
  if (is.factor(x)) tab <- tab %>% mutate(level = factor(level, levels = levels(x))) %>% arrange(level)
  tab %>%
    mutate(level = as.character(level), pct = pct(n / sum(n), 1)) %>%
    bind_rows(tibble(level = "Missing", n = sum(is.na(x)), pct = NA_real_)) %>%
    mutate(variable = var, .before = 1)
}

describe_cont <- function(var) {
  x <- d[[var]]
  tibble(variable = var, n = sum(!is.na(x)), missing = sum(is.na(x)),
         mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
         median = median(x, na.rm = TRUE), q1 = quantile(x, 0.25, na.rm = TRUE),
         q3 = quantile(x, 0.75, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)) %>%
    mutate(across(mean:max, ~ round(.x, 1)))
}

cat_vars <- c("sex", "lym_nhl", "cart_product", "cart_line", "bridge_yn", "asct", "cns_current",
              "ecog", "karnofsky", "cfs_cat", "ves_cat", "gait_cat", "crs_yn", "crs_highestgrade",
              "icans_yn", "icans_highest", "icu_yn")
cont_vars <- c("age", "ldh", "crp", "albumin", "hctci_score", "cirs_total", "ves13_score", "cfs_score",
               "grip_average_kg", "walk_mpers", "minicog_score", "phq_total_score", "los_days")

table1_cat  <- map_dfr(cat_vars, describe_cat)
table1_cont <- map_dfr(cont_vars, describe_cont)
print(table1_cont, n = Inf)
save_table(table1_cat, "table1_categorical")
save_table(table1_cont, "table1_continuous")

## ---- Key cohort numbers for the abstract --------------------------------------------

record("n_patients", N)
record("n_visits", nrow(al), note = "assessment visits (repeated-measures rows)")
record("age_median", median(d$age), lo = min(d$age), hi = max(d$age), note = "lo/hi = range")
record("male_pct", pct(mean(d$sex == "Male")))
nhl_known <- d$lym_nhl[!is.na(d$lym_nhl)]
record("dlbcl_pct", pct(mean(nhl_known == "DLBCL")), note = "of patients with known NHL subtype")
record("tfl_pct", pct(mean(nhl_known == "Transformed DLBCL from follicular lymphoma")), note = "of known subtype")
record("axicel_pct", pct(mean(d$cart_product == "Yescarta")))
record("third_line_pct", pct(mean(d$cart_line == 3)))
record("bridged_pct", pct(mean(d$bridge_yn == "Yes")))
record("cfs_baseline_median", median(d$cfs_score))
record("cfs_gt3_pct", pct(mean(d$cfs_score > 3)))
record("crs_any_pct", pct(mean(d$crs_any)))
record("crs_g2_pct", pct(mean(d$crs_g2)))
record("icans_any_pct", pct(mean(d$icans_any)))
record("icu_pct", pct(mean(d$icu)))
record("los_median", median(d$los_days), lo = quantile(d$los_days, 0.25), hi = quantile(d$los_days, 0.75),
       note = "lo/hi = IQR")

## ---- Events and causes of death -------------------------------------------------------

events <- d %>%
  summarise(patients = n(), deaths = sum(os_event), progressions = sum(progressed),
            pfs_events = sum(pfs_event), rm = sum(cause == "RM"), nrm = sum(cause == "NRM"),
            unknown_cause = sum(cause == "Unknown"),
            deaths_after_progression = sum(cr_prog == 1), deaths_without_progression = sum(cr_prog == 2))
print(events)
save_table(events, "events")

record("deaths", events$deaths)
record("deaths_rm", events$rm)
record("deaths_nrm", events$nrm)
record("deaths_unknown", events$unknown_cause)
record("pfs_events", events$pfs_events)

causes <- d %>%
  filter(os_event == 1) %>%
  count(cause, cause_source, death_reason = coalesce(death_reason, "(not recorded)")) %>%
  arrange(cause, desc(n))
print(causes, n = Inf)
save_table(causes, "causes_of_death")

# NRM by broad category (used in the abstract)
nrm_groups <- d %>%
  filter(cause == "NRM") %>%
  mutate(group = case_when(
    str_detect(str_to_lower(death_reason), "covid") ~ "COVID-19",
    str_detect(str_to_lower(death_reason), "leuk|mds|lymphoma") ~ "Second myeloid/lymphoid neoplasm",
    str_detect(str_to_lower(death_reason), "maid") ~ "MAID",
    TRUE ~ "Other / unexplained")) %>%
  count(group) %>% mutate(p = 100*n/sum(n))
print(nrm_groups)
save_table(nrm_groups, "nrm_categories")

## ---- Follow-up, OS, PFS -------------------------------------------------------------------

# Median follow-up: reverse Kaplan-Meier (censoring is the "event").
fu <- survfit(Surv(os_months, 1 - os_event) ~ 1, data = d)
fu_q <- quantile(fu, probs = 0.5, conf.int = TRUE)
record("followup_median_months", fu_q$quantile, lo = fu_q$lower, hi = fu_q$upper, note = "reverse KM")

km_os  <- survfit(Surv(os_months, os_event) ~ 1, data = d)
km_pfs <- survfit(Surv(pfs_months, pfs_event) ~ 1, data = d)
surv_est <- bind_rows(
  km_at(km_os, c(12, 24, 36)) %>% mutate(outcome = "OS"),
  km_at(km_pfs, c(12, 24, 36)) %>% mutate(outcome = "PFS")
) %>% relocate(outcome)

# Cumulative incidence of RM (cause 1) and NRM (cause 2), primary definition.
cif <- cif_at(d$os_months, d$cr_cause, c(12, 24, 36)) %>%
  filter(cause %in% 1:2) %>%
  transmute(outcome = if_else(cause == 1, "CIF relapse-related death", "CIF non-relapse death"),
            strata = "All", time, surv = cif)
# Sensitivity: standard definition (death without vs after progression)
cif_prog <- cif_at(d$os_months, d$cr_prog, c(12, 24, 36)) %>%
  transmute(outcome = if_else(cause == 1, "CIF death after progression", "CIF death without progression"),
            strata = "All", time, surv = cif)

surv_table <- bind_rows(surv_est, cif, cif_prog) %>% mutate(across(c(surv, lo, hi), ~ round(100 * .x, 1)))
print(surv_table, n = Inf)
save_table(surv_table, "survival_estimates")

os24  <- surv_est %>% filter(outcome == "OS", time == 24)
pfs24 <- surv_est %>% filter(outcome == "PFS", time == 24)
record("os_24m_pct", pct(os24$surv, 1), lo = pct(os24$lo, 1), hi = pct(os24$hi, 1))
record("pfs_24m_pct", pct(pfs24$surv, 1), lo = pct(pfs24$lo, 1), hi = pct(pfs24$hi, 1))
record("cif_rm_24m_pct", pct(cif$surv[cif$time == 24 & str_detect(cif$outcome, "relapse-related")], 1))
record("cif_nrm_24m_pct", pct(cif$surv[cif$time == 24 & str_detect(cif$outcome, "non-relapse")], 1))

message("03_descriptives.R complete.")
