# ==============================================================================
# 04_derive_survival_outcomes.R
#
# Purpose : Derive overall-survival (OS) and progression-free-survival (PFS)
#           time-to-event variables from the survival sheet and the baseline
#           infusion date. Deliberately outputs an outcomes-only table keyed
#           by study_id (not merged with baseline covariates) - joining
#           belongs in 05_merge_analysis_datasets.R, so that step isn't
#           duplicated here and in every downstream script.
# Inputs  : data/processed/survival_updated_raw.rds
#           data/processed/baseline_clean.rds   (date_infusion)
# Outputs : data/processed/analysis_survival.rds
# Depends : 00_packages.R, 01_import_raw.R, 02_clean_baseline.R
#
# A note on two fields NOT used here
# -------------------------------------------------
#   - `5 year_survival`: despite the name, this behaves like a *current*
#     vital-status flag (1 = alive) rather than a landmark 5-year survival
#     indicator - e.g. two patients with a recorded death_date are flagged
#     "1". os_event is derived directly from death_date instead, which is
#     unambiguous. Flagged in data/data_dictionary.md for the study team.
#   - study_id 76: present in the prior extract with a recorded death, but
#     absent from the updated extract entirely. Not this script's place to
#     guess why; excluded because it isn't in survival_updated_raw at all.
# ==============================================================================

source(here::here("scripts", "00_packages.R"))

survival_raw <- readRDS(here("data", "processed", "survival_updated_raw.rds"))
baseline_clean <- readRDS(here("data", "processed", "baseline_clean.rds"))


glimpse(survival_raw)

## ---- Derive OS and PFS ----------------------------------------------------

# TODO this code does not conduct an initial exploration of date order or possible
# discrepancies, such as follow-up dates after death dates, etc (more for the
# longitudinal data exploration). However, prior validations seemed OK.

analysis_survival <- 
baseline_clean %>%
  select(study_id, date_infusion) %>%
  inner_join(survival_raw, by = "study_id") %>%
  mutate(
    across(c(eos_dov, prog_date, death_date, last_followup), as.Date),

    death_event = as.integer(!is.na(death_date)),
    prog_event  = as.integer(!is.na(prog_date)),

    # last known contact: death date if it occurred, otherwise last recorded
    # follow-up
    last_contact = pmax(death_date, last_followup, na.rm = TRUE),

    os_days   = as.numeric(difftime(last_contact, date_infusion, units = "days")),
    os_months = os_days / 30.4375,   # average days/month, avoids a fixed 30- or 31-day assumption
    os_event  = death_event,

    # PFS event = progression or death, whichever occurs first; PFS time
    # runs to that date, or to last contact if neither occurred
    pfs_date   = pmin(prog_date, death_date, na.rm = TRUE),
    pfs_days   = as.numeric(difftime(
      if_else(!is.na(pfs_date), pfs_date, last_contact),
      date_infusion,
      units = "days"
    )),
    pfs_months = pfs_days / 30.4375,
    pfs_event  = as.integer(!is.na(pfs_date))
  ) %>%
  select(
    study_id, date_infusion, last_contact,
    os_days, os_months, os_event,
    pfs_days, pfs_months, pfs_event,
    death_disease,   # 0 = not disease-related, 1 = disease-related, 2 = unknown - see dictionary
    death_reason, eos_reason
  )

analysis_survival

## ---- Sanity checks ---------------------------------------------------

stopifnot(
  "duplicate study_id in analysis_survival" = !anyDuplicated(analysis_survival$study_id),
  "negative os_months" = all(analysis_survival$os_months >= 0, na.rm = TRUE),
  "negative pfs_months" = all(analysis_survival$pfs_months >= 0, na.rm = TRUE),
  "missing os_months" = !anyNA(analysis_survival$os_months),
  "pfs time shorter than os time" = all(analysis_survival$pfs_months <= analysis_survival$os_months + 1e-6)
)

message("--- Event counts ---")
message("N patients:        ", nrow(analysis_survival))
message("Deaths (OS events): ", sum(analysis_survival$os_event))
message("PFS events (progression or death): ", sum(analysis_survival$pfs_event))

message("\n--- Follow-up summary (months) ---")
fu_summary <- analysis_survival %>%
  summarise(
    median_os_all = median(os_months),
    max_os = max(os_months),
    median_os_censored = median(os_months[os_event == 0])
  )
message(paste(capture.output(print(fu_summary)), collapse = "\n"))

message("\ndeath_disease among deaths (0 = not disease-related, 1 = disease-related, 2 = unknown, NA = not recorded):")
message(paste(capture.output(print(table(
  analysis_survival$death_disease[analysis_survival$os_event == 1], useNA = "ifany"
))), collapse = "\n"))

## ---- Save ---------------------------------------------------------------

saveRDS(analysis_survival, here("data", "processed", "analysis_survival.rds"))

message("\n04_derive_survival_outcomes.R complete. Wrote data/processed/analysis_survival.rds")
