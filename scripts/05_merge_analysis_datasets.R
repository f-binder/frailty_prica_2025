# ==============================================================================
# 05_merge_analysis_datasets.R
#
# Purpose : Join the three cleaned building blocks (baseline covariates,
#           repeated-measures frailty assessments, survival outcomes) into
#           two final analysis-ready objects. Nothing is recoded or derived
#           here beyond the join itself - scripts 06+ do their own
#           analysis-specific derivations (e.g. dichotomising CFS) visibly,
#           at the point of use, rather than baking assumptions in here.
# Inputs  : data/processed/baseline_clean.rds
#           data/processed/repeated_measures_clean.rds
#           data/processed/analysis_survival.rds
# Outputs : data/processed/analysis_wide.rds   (one row per patient)
#           data/processed/analysis_long.rds   (one row per patient x visit)
# Depends : 00_packages.R, 01_import_raw.R, 02_clean_baseline.R,
#           03_clean_repeated_measures.R, 04_derive_survival_outcomes.R
#
# -------------------------------------------------
#
# Why two objects:
# analysis_wide is what a "baseline data" analysis needs (07 toxicity
# models, 08 baseline Cox models): one row per patient, with the baseline
# clinical covariates AND the frailty-instrument scores as measured at the
# baseline visit (these live in the repeated-measures sheet, not the
# baseline sheet itself, since the same instruments are re-administered on
# follow-up - see the join below).
#
# analysis_long is what a longitudinal analysis needs (09 time-varying Cox,
# 10 trajectory modelling): one row per patient x visit, with each
# patient's eventual OS/PFS time and event status attached to every one of
# their visit rows, so a downstream script doesn't have to repeat that join
# every time it wants to, say, colour a trajectory plot by vital status.
# It deliberately does NOT carry the full baseline covariate panel - 09
# reads that directly from baseline_clean when it builds its counting-
# process (tmerge) skeleton, since tmerge attaches time-constant covariates
# at that step, not via the time-varying data source.
# ==============================================================================

source(here::here("scripts", "00_packages.R"))

baseline_clean <- readRDS(here("data", "processed", "baseline_clean.rds"))
repeated_measures_clean <- readRDS(here("data", "processed", "repeated_measures_clean.rds"))
analysis_survival <- readRDS(here("data", "processed", "analysis_survival.rds"))

## ---- analysis_wide: one row per patient ----------------------------------

# Frailty-instrument scores as measured at the baseline visit (visit_month
# == 0). Verified 1:1 against baseline_clean before relying on it below:
# every patient has exactly one baseline-timepoint row.
baseline_frailty <- repeated_measures_clean %>%
  filter(visit_month == 0) %>%
  select(
    study_id, cfs_score, grip_average_kg, walk_time, walk_mpers,
    minicog_score, phq_total_score, phq_10
  )

stopifnot(
  "baseline_frailty does not have exactly one row per baseline_clean patient" =
    setequal(baseline_frailty$study_id, baseline_clean$study_id) &&
    !anyDuplicated(baseline_frailty$study_id)
)

# analysis_survival carries its own date_infusion (joined in during 04,
# from this same baseline_clean) - dropped here to avoid a .x/.y collision,
# since baseline_clean's copy is authoritative.
survival_for_join <- analysis_survival %>% select(-date_infusion)

analysis_wide <- baseline_clean %>%
  inner_join(baseline_frailty, by = "study_id") %>%
  inner_join(survival_for_join, by = "study_id")

stopifnot(
  "analysis_wide row count does not match baseline_clean" =
    nrow(analysis_wide) == nrow(baseline_clean),
  "duplicate study_id in analysis_wide" = !anyDuplicated(analysis_wide$study_id)
)

message("--- analysis_wide ---")
message("Rows: ", nrow(analysis_wide), " | Cols: ", ncol(analysis_wide))
message("OS events: ", sum(analysis_wide$os_event), " | PFS events: ", sum(analysis_wide$pfs_event))

## ---- analysis_long: one row per patient x visit ---------------------------

outcomes_for_join <- analysis_survival %>%
  select(study_id, os_months, os_event, pfs_months, pfs_event)

analysis_long <- repeated_measures_clean %>%
  left_join(outcomes_for_join, by = "study_id")

stopifnot(
  "analysis_long row count does not match repeated_measures_clean" =
    nrow(analysis_long) == nrow(repeated_measures_clean),
  "analysis_long has visits occurring after OS time" =
    all(analysis_long$visit_month <= analysis_long$os_months + 1e-6)
)

message("\n--- analysis_long ---")
message("Rows: ", nrow(analysis_long), " | Cols: ", ncol(analysis_long))
message("Patients: ", n_distinct(analysis_long$study_id))

## ---- Save ---------------------------------------------------------------

saveRDS(analysis_wide, here("data", "processed", "analysis_wide.rds"))
saveRDS(analysis_long, here("data", "processed", "analysis_long.rds"))

message("\n05_merge_analysis_datasets.R complete. Wrote analysis_wide.rds and analysis_long.rds")
