# ==============================================================================
# 06_descriptive_tables.R
#
# Purpose : Table 1 (baseline characteristics), follow-up summary, and
#           event counts - reproducing the structure of the study's own
#           descriptive-statistics report (Stats-Frailty_and_CAR-T,
#           1-Aug-2025), on the matured, codebook-recoded cohort.
# Inputs  : data/processed/analysis_wide.rds
#           data/processed/survival_updated_raw.rds  (progression date only
#           - see note below)
# Outputs : output/tables/table1_categorical.csv
#           output/tables/table1_continuous.csv
#           output/tables/followup_summary.csv
#           output/tables/event_counts.csv
#           output/tables/death_cause_summary.csv
# Depends : 00_packages.R, ..., 05_merge_analysis_datasets.R
#
# A note on progression status
# -------------------------------------------------
# `analysis_wide` carries `pfs_event` (progression OR death, whichever
# first), but not a standalone "did this patient progress" flag - that
# intermediate was computed in 04_derive_survival_outcomes.R and
# deliberately not kept in its output, since PFS scripts only need the
# composite. Reproducing the report's own progression x vital-status
# cross-tab needs that standalone flag, so it's recomputed here, directly
# from prog_date in the raw survival sheet, rather than by editing 04.
# ==============================================================================

source(here::here("scripts", "00_packages.R"))

analysis_wide <- readRDS(here("data", "processed", "analysis_wide.rds"))
survival_updated_raw <- readRDS(here("data", "processed", "survival_updated_raw.rds"))

tables_dir <- here("output", "tables")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

## ---- Human-readable labels for export -----------------------------------
# Keeps the raw REDCap-style column names in the code (so they still match
# the data dictionary and every other script) while making the exported
# CSVs self-explanatory without a cross-reference.

var_labels <- c(
  sex = "Sex", diagnosis = "Diagnosis", lym_nhl = "Lymphoma subtype (NHL)",
  cns_current = "Current CNS disease", active_mal = "Another active malignancy",
  asct = "Prior autologous stem cell transplant (ASCT)",
  allosct_yn = "Prior allogeneic stem cell transplant",
  karnofsky = "Karnofsky performance status (%)", ecog = "ECOG performance status",
  bridge_yn = "Bridging therapy (any)", 
  bridge___0 = "Bridging chemotherapy", bridge___1 = "Bridging radiation", 
  bridge___2 = "Bridging steroids",
  cart_product = "CAR-T product", crs_yn = "CRS (any grade)",
  crs_highestgrade = "Highest CRS grade", icans_yn = "ICANS (any grade)",
  icans_highest = "Highest ICANS grade", icu_yn = "ICU admission",
  age = "Age (years)", ldh = "LDH (U/L)", crp = "CRP (mg/L)", albumin = "Albumin (g/L)",
  hctci_score = "HCT-CI score", cirs_total = "CIRS total score", ves13_score = "VES-13 score",
  los_days = "Length of hospital stay (days)",
  cfs_score = "Clinical Frailty Scale (baseline)",
  grip_average_kg = "Grip strength, average (kg, baseline)",
  walk_time = "4m walk time (seconds, baseline)",
  walk_mpers = "4m walk speed (m/s, baseline)",
  minicog_score = "Mini-Cog score (baseline)",
  phq_total_score = "PHQ-9 total score (baseline)"
)

## ---- Helpers --------------------------------------------------------

#' Look up a human-readable label, falling back to the raw column name if
#' it isn't in var_labels. `var_labels[var]` (single bracket) rather than
#' `[[var]]` is used deliberately - `[[` errors on a missing name in a
#' named atomic vector instead of returning NA, which would defeat the
#' fallback.
get_label <- function(var) {
  lbl <- unname(var_labels[var])
  if (is.na(lbl)) var else lbl
}

#' Summarise one categorical/discrete column as n (%) among non-missing,
#' plus a separate "Missing" row (raw count, no percentage) - matching the
#' style of the study's own descriptive-statistics report. count() (not a
#' pre-coercion to character) is used so the row order follows the column's
#' natural order (ascending for numeric, level order for factors) rather
#' than alphabetical string order, which would e.g. misorder Karnofsky's
#' two-digit percentage values.
describe_cat <- function(data, var) {
  n_missing <- sum(is.na(data[[var]]))

  tab <- data %>%
    filter(!is.na(.data[[var]])) %>%
    count(.data[[var]], name = "n") %>%
    rename(level = 1) %>%
    mutate(level = as.character(level)) %>%
    add_pcump() %>%
    select(level, n, pct = p)

  bind_rows(tab, tibble(level = "Missing", n = n_missing, pct = NA_real_)) %>%
    mutate(
      variable = var,
      label = get_label(var),
      pct = round(100 * pct, 1),
      .before = 1
    )
}

#' Summarise one continuous column as mean (SD), median (min, max), and a
#' missing count.
describe_cont <- function(data, var) {
  x <- data[[var]]
  tibble(
    variable = var,
    label = get_label(var),
    n = sum(!is.na(x)),
    missing = sum(is.na(x)),
    mean = round(mean(x, na.rm = TRUE), 1),
    sd = round(sd(x, na.rm = TRUE), 1),
    median = round(median(x, na.rm = TRUE), 1),
    min = round(min(x, na.rm = TRUE), 1),
    max = round(max(x, na.rm = TRUE), 1)
  )
}

## ---- Table 1: categorical / discrete variables --------------------------

categorical_vars <- c(
  "sex", "diagnosis", "lym_nhl", "cns_current", "active_mal",
  "asct", "allosct_yn", "karnofsky", "ecog",
  "bridge_yn", "bridge___0", "bridge___1", "bridge___2",
  "cart_product", "crs_yn", "crs_highestgrade", "icans_yn", "icans_highest",
  "icu_yn"
)

table1_categorical <- 
  map_dfr(categorical_vars, ~ describe_cat(analysis_wide, .x))

message("--- Table 1: categorical (spot-check, first 20 rows) ---")
print(table1_categorical %>% select(label, level, n, pct), n = 20)

## ---- Table 1: continuous variables ---------------------------------------

continuous_vars <- c(
  "age", "ldh", "crp", "albumin", "hctci_score", "cirs_total", "ves13_score",
  "los_days", "cfs_score", "grip_average_kg", "walk_time", "walk_mpers",
  "minicog_score", "phq_total_score"
)

# are all "continuous" variables actually numeric? CRP was corrected on data import
stopifnot(
  "all expected continuous variables are numeric" =
    all(map_lgl(analysis_wide %>% select(all_of(continuous_vars)), is.numeric))
)

   # create table describing continuous variables
table1_continuous <- map_dfr(continuous_vars, ~ describe_cont(analysis_wide, .x))

message("\n--- Table 1: continuous ---")
print(table1_continuous %>% select(label, n, missing, mean, sd, median, min, max))


## ---- Follow-up summary ------------------------------------------------

followup_summary <- tibble(
  metric = c("OS follow-up (months)", "PFS follow-up (months)"),
  n = nrow(analysis_wide),
  mean = c(mean(analysis_wide$os_months), mean(analysis_wide$pfs_months)),
  sd = c(sd(analysis_wide$os_months), sd(analysis_wide$pfs_months)),
  median = c(median(analysis_wide$os_months), median(analysis_wide$pfs_months)),
  min = c(min(analysis_wide$os_months), min(analysis_wide$pfs_months)),
  max = c(max(analysis_wide$os_months), max(analysis_wide$pfs_months))
) %>%
  mutate(across(c(mean, sd, median, min, max), ~ round(.x, 1)))

message("\n--- Follow-up summary ---")
print(followup_summary)

## ---- Event counts ------------------------------------------------------

progression_flag <- survival_updated_raw %>%
  transmute(study_id, progressed = if_else(!is.na(prog_date), "Yes", "No"))

event_data <- analysis_wide %>%
  left_join(progression_flag, by = "study_id") %>%
  mutate(vital_status = if_else(os_event == 1, "Deceased", "Alive"))

glimpse(event_data)

vital_status_counts <- event_data %>% count(category = "Vital status", level = vital_status) %>% add_pcump() %>% select(category, level, n, p) %>% rename(pct = p)
progression_counts  <- event_data %>% count(category = "Progression", level = progressed) %>% add_pcump() %>% select(category, level, n, p) %>% rename(pct = p)
pfs_event_counts    <- event_data %>%
  mutate(level = if_else(pfs_event == 1, "Progression or death", "Alive, no progression")) %>%
  count(category = "PFS event", level) %>% add_pcump() %>%
  select(category, level, n, p) %>% rename(pct = p)

event_counts <- bind_rows(vital_status_counts, progression_counts, pfs_event_counts) %>%
  mutate(pct = round(100 * pct, 1))

message("\n--- Event counts ---")
print(event_counts)

message("\n--- Event counts: progression and death ---")
event_data %>%
  count(progressed, vital_status) %>%
  add_count(progressed, wt = n, name = "progression_status_n") %>%
  relocate(progressed, progression_status_n) %>%
  add_pcump()

message("\n--- Progression x vital status ---") # a shorter version of the above
progression_by_vital <- event_data %>% count(progressed, vital_status)
print(progression_by_vital)

## ---- Reason for death ---------------------------------------------------
# death_disease coding: the code book only formally defines 1 = Yes,
# 0 = No; the "2 = Unknown" level is not in the code book itself but is
# confirmed by the study's own descriptive-statistics report ("2-Unknown").
# Labelled here for the table only - analysis_wide keeps the raw numeric
# code, since later scripts (competing-risk analyses) need it as such.

# TODO here, transformation should count as relapse-related mortality -
# we need to review one case of T cell lymphoma.

glimpse(event_data)

event_data %>% count(death_disease, death_reason)

event_data %>%
  filter(str_detect(tolower(death_reason), fixed("t cell"))) %>%
  glimpse()


death_cause_summary <- event_data %>%
  filter(os_event == 1) %>%
  mutate(
    death_disease_label = case_when(
      death_disease == 1 ~ "Disease-related (relapse)",
      death_disease == 0 ~ "Not disease-related",
      death_disease == 2 ~ "Unknown",
      TRUE ~ "Not recorded"
    )
  ) %>%
  count(level = death_disease_label, name = "n") %>%
  add_pcump() %>%
  mutate(pct = round(100 * p, 1)) %>%
  select(level, n, pct)

message("\n--- Cause of death (among deaths) ---")
print(death_cause_summary)

## ---- Save ---------------------------------------------------------------

write_csv(table1_categorical, file.path(tables_dir, "table1_categorical.csv"))
write_csv(table1_continuous, file.path(tables_dir, "table1_continuous.csv"))
write_csv(followup_summary, file.path(tables_dir, "followup_summary.csv"))
write_csv(event_counts, file.path(tables_dir, "event_counts.csv"))
write_csv(death_cause_summary, file.path(tables_dir, "death_cause_summary.csv"))

message("\n06_descriptive_tables.R complete. Wrote 5 CSVs to output/tables/")
