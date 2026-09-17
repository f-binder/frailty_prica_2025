# ==============================================================================
# 03_clean_repeated_measures.R
#
# Purpose : Clean the longitudinal (one row per patient per visit) frailty
#           assessment sheet: map the text timepoint label to a numeric
#           month for modelling, enforce one row per patient x visit, and
#           check for out-of-range scores on the bounded instruments.
# Inputs  : data/processed/repeated_measures_updated_raw.rds
#           data/processed/baseline_clean.rds   (study_id list, for a
#                                                 referential-integrity check)
# Outputs : data/processed/repeated_measures_clean.rds
# Depends : 00_packages.R, 01_import_raw.R, 02_clean_baseline.R
# ==============================================================================

source(here::here("scripts", "00_packages.R"))

rm_raw <- readRDS(here("data", "processed", "repeated_measures_updated_raw.rds"))
baseline_clean <- readRDS(here("data", "processed", "baseline_clean.rds"))

## ---- Timepoint -> numeric visit month -------------------------------------
# The assessment schedule is fixed (baseline, 1, 3, 6, 12 months); encoding
# it as a number rather than only a factor is what the trajectory/mixed
# models need visit_month for.

visit_month_map <- c(
  "baseline"   = 0,
  "1 month"    = 1,
  "3 months"   = 3,
  "6 months"   = 6,
  "12 months"  = 12
)

unmapped <- setdiff(unique(rm_raw$timepoint), names(visit_month_map))
if (length(unmapped) > 0) {
  stop(
    "Unrecognised timepoint value(s) not in visit_month_map: ",
    paste(unmapped, collapse = ", "),
    " - update visit_month_map before proceeding."
  )
}

repeated_measures_clean <- rm_raw %>%
  mutate(
    study_id = as.integer(study_id),
    timepoint = factor(timepoint, levels = names(visit_month_map)),
    visit_month = recode(timepoint, !!!visit_month_map),
    visit_month = as.numeric(visit_month),
    dov = as.Date(dov)
  ) %>%
  arrange(study_id, visit_month) %>%
  relocate(study_id, timepoint, visit_month, dov)

## ---- Restrict to patients retained in the cleaned baseline ---------------
# One patient present in the prior extract's repeated-measures sheet was
# dropped from the updated baseline (study_id 76); this keeps the two
# tables consistent going forward rather than assuming it.

dropped_ids <- setdiff(unique(repeated_measures_clean$study_id), baseline_clean$study_id)
if (length(dropped_ids) > 0) {
  message(
    "Dropping repeated-measures rows for study_id(s) not present in ",
    "baseline_clean: ", paste(dropped_ids, collapse = ", ")
  )
  repeated_measures_clean <- repeated_measures_clean %>%
    filter(study_id %in% baseline_clean$study_id)
}

## ---- Sanity checks ---------------------------------------------------

stopifnot(
  "duplicate study_id x visit_month combination" =
    !anyDuplicated(repeated_measures_clean[, c("study_id", "visit_month")]),
  "cfs_score out of the instrument's 1-9 range" =
    all(dplyr::between(repeated_measures_clean$cfs_score, 1, 9), na.rm = TRUE),
  "minicog_score out of the instrument's 0-5 range" =
    all(dplyr::between(repeated_measures_clean$minicog_score, 0, 5), na.rm = TRUE)
)

n_obs_per_patient <- repeated_measures_clean %>%
  filter(!is.na(cfs_score)) %>%
  count(study_id, name = "n_cfs_obs")

message("--- Visits per patient (non-missing CFS) ---")
message(paste(capture.output(print(table(n_obs_per_patient$n_cfs_obs))), collapse = "\n"))

message("\n--- CFS score by visit month ---")
cfs_by_visit <- repeated_measures_clean %>%
  group_by(visit_month) %>%
  summarise(
    n = sum(!is.na(cfs_score)),
    mean = round(mean(cfs_score, na.rm = TRUE), 2),
    sd = round(sd(cfs_score, na.rm = TRUE), 2),
    .groups = "drop"
  )
message(paste(capture.output(print(cfs_by_visit)), collapse = "\n"))

message("\nRows: ", nrow(repeated_measures_clean), " | Patients: ", n_distinct(repeated_measures_clean$study_id))

## ---- Save ---------------------------------------------------------------

saveRDS(repeated_measures_clean, here("data", "processed", "repeated_measures_clean.rds"))

message("\n03_clean_repeated_measures.R complete. Wrote data/processed/repeated_measures_clean.rds")
