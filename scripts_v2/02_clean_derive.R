# ==============================================================================
# 02_clean_derive.R
#
# Purpose : Clean the three raw sheets and derive every analysis variable:
#             - baseline: code-book factors, Karnofsky %, derived categories
#             - repeated measures: visit month, actual visit time, protocol
#               eligibility for quality-of-life questionnaires
#             - survival: OS, PFS, cause of death (reviewed column supersedes
#               the original), competing-risk codes under two definitions
#             - data-quality flags written to a table for the study team
#           and build the analysis datasets.
# Inputs  : data/processed/v2/{baseline_raw, rm_raw, survival_raw}.rds
# Outputs : data/processed/v2/{baseline, rm, survival, analysis_wide, analysis_long}.rds
#           output/v2/tables/data_quality_flags.csv
#
# Codings follow the REDCap code book (Frailty - Code Book - 09Jul2025.docx).
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "02_clean_derive.R"
section("02 - Cleaning and derived variables")

baseline_raw <- read_proc("baseline_raw")
rm_raw       <- read_proc("rm_raw")
survival_raw <- read_proc("survival_raw")

yn  <- function(x) factor(x, levels = c(0, 1), labels = c("No", "Yes"))
ynu <- function(x) factor(x, levels = c(0, 1, 2), labels = c("No", "Yes", "Unknown"))

## ---- Baseline ------------------------------------------------------------------------

baseline <- baseline_raw %>%
  mutate(
    study_id      = as.integer(study_id),
    sex           = factor(sex, levels = 0:3, labels = c("Male", "Female", "Other", "Not reported")),
    diagnosis     = factor(diagnosis, levels = 0:2, labels = c("Lymphoma", "Myeloma", "Leukemia")),
    lym_diagnosis = factor(lym_diagnosis, levels = 0:1, labels = c("Non-Hodgkin lymphoma", "Other")),
    lym_nhl       = factor(lym_nhl, levels = 0:5, labels = c(
      "DLBCL", "High-grade B-cell lymphoma", "Primary mediastinal B-cell lymphoma",
      "Transformed DLBCL from follicular lymphoma", "Follicular lymphoma", "Mantle cell lymphoma")),
    rel_trans_yn  = ynu(rel_trans_yn),
    cns_current   = ynu(cns_current),
    cart_product  = factor(cart_product, levels = 0:3, labels = c("Kymriah", "Yescarta", "JCAR Trial", "Tecartus")),
    across(c(crs_yn, icans_yn, icu_yn, bridge_yn, asct, asct_relapse, active_mal, allosct_yn), yn),
    karnofsky     = as.integer(karnofsky) * 10L,   # stored as a 1-10 index
    ecog          = as.integer(ecog),
    cart_line     = as.integer(cart_line),
    crs_highestgrade = as.integer(crs_highestgrade),
    icans_highest = as.integer(icans_highest),
    date_infusion = as.Date(date_infusion),
    los_days      = as.numeric(los_days),
    # free-text bridging regimens: harmonise the polatuzumab spellings
    bridge_chemoreg = case_when(
      str_detect(bridge_chemoreg, regex("\\bpola", ignore_case = TRUE)) &
        str_detect(bridge_chemoreg, regex("\\bbr\\b", ignore_case = TRUE)) ~ "Polatuzumab-BR",
      str_detect(bridge_chemoreg, regex("\\bpola", ignore_case = TRUE)) ~ "Polatuzumab-R",
      TRUE ~ bridge_chemoreg)
  )

stopifnot(
  "date_infusion missing" = !anyNA(baseline$date_infusion),
  "Karnofsky outside 10-100" = all(between(baseline$karnofsky, 10, 100), na.rm = TRUE)
)

## ---- Repeated measures ---------------------------------------------------------------

unmapped <- setdiff(unique(rm_raw$timepoint), names(VISITS))
if (length(unmapped) > 0) stop("Unknown timepoint label(s): ", paste(unmapped, collapse = ", "))

dropped <- setdiff(unique(rm_raw$study_id), baseline$study_id)
if (length(dropped) > 0) message("Repeated-measures rows dropped (study_id not in baseline): ", paste(dropped, collapse = ", "))

rm <- rm_raw %>%
  filter(study_id %in% baseline$study_id) %>%
  mutate(
    study_id    = as.integer(study_id),
    visit_month = unname(VISITS[timepoint]),
    visit       = factor(timepoint, levels = names(VISITS), labels = VISIT_LABELS),
    dov         = as.Date(dov)
  ) %>%
  left_join(baseline %>% select(study_id, date_infusion), by = "study_id") %>%
  group_by(study_id) %>%
  mutate(enrol_date = dov[visit_month == 0][1]) %>%
  ungroup() %>%
  mutate(
    actual_month = as.numeric(dov - date_infusion) / DAYS_PER_MONTH,
    # QoL questionnaires (EORTC, EQ-5D) only required from protocol v. 27 Apr 2022
    qol_required = enrol_date >= QOL_PROTOCOL_START
  ) %>%
  arrange(study_id, visit_month) %>%
  relocate(study_id, timepoint, visit, visit_month, dov, actual_month)

stopifnot(
  "duplicate patient x visit" = !anyDuplicated(rm[, c("study_id", "visit_month")]),
  "every patient has a baseline visit" = setequal(rm$study_id[rm$visit_month == 0], baseline$study_id),
  "CFS outside 1-9" = all(between(rm$cfs_score, 1, 9), na.rm = TRUE),
  "Mini-Cog outside 0-5" = all(between(rm$minicog_score, 0, 5), na.rm = TRUE)
)

## ---- Survival outcomes and cause of death -------------------------------------------
# OS: infusion to death; censored at last contact (latest of death or last
#     follow-up date).
# PFS: infusion to progression or death, whichever first; censored at last
#      contact. (See data-quality flag "progression ascertainment" below.)
# Cause of death: reviewed_death_disease supersedes death_disease when filled.
#   1 = relapse-related (RM), 0 = non-relapse (NRM), 2 / blank = unknown.
# Two competing-risk definitions are kept:
#   cr_cause : 0 alive, 1 RM, 2 NRM, 3 unknown       (cause-of-death attribution; primary)
#   cr_prog  : 0 alive, 1 death after progression,
#              2 death without prior progression     (standard NRM definition; sensitivity)

survival <- survival_raw %>%
  mutate(
    study_id = as.integer(study_id),
    across(c(eos_dov, prog_date, death_date, last_followup), as.Date),
    death_disease_final = coalesce(reviewed_death_disease, death_disease),
    cause_source = case_when(!is.na(reviewed_death_disease) ~ "reviewed",
                             !is.na(death_disease) ~ "original", TRUE ~ "none")
  ) %>%
  inner_join(baseline %>% select(study_id, date_infusion), by = "study_id") %>%
  mutate(
    os_event     = as.integer(!is.na(death_date)),
    last_contact = pmax(death_date, last_followup, na.rm = TRUE),
    os_months    = as.numeric(last_contact - date_infusion) / DAYS_PER_MONTH,
    pfs_date     = pmin(prog_date, death_date, na.rm = TRUE),
    pfs_event    = as.integer(!is.na(pfs_date)),
    pfs_months   = as.numeric(coalesce(pfs_date, last_contact) - date_infusion) / DAYS_PER_MONTH,
    progressed   = as.integer(!is.na(prog_date)),
    cause = case_when(os_event == 0 ~ "Alive",
                      death_disease_final == 1 ~ "RM",
                      death_disease_final == 0 ~ "NRM",
                      TRUE ~ "Unknown"),
    cause = factor(cause, levels = c("Alive", "RM", "NRM", "Unknown")),
    cr_cause = as.integer(cause) - 1L,
    prog_before_death = os_event == 1 & !is.na(prog_date) & prog_date < death_date,
    cr_prog = case_when(os_event == 0 ~ 0L, prog_before_death ~ 1L, TRUE ~ 2L)
  )

stopifnot(
  "OS time missing" = !anyNA(survival$os_months),
  "negative OS time" = all(survival$os_months >= 0),
  "PFS longer than OS" = all(survival$pfs_months <= survival$os_months + 1e-9)
)

message("Cause of death (deaths only): ",
        paste(names(table(survival$cause[survival$os_event == 1])),
              table(survival$cause[survival$os_event == 1]), sep = " = ", collapse = "; "),
        " | source: ", paste(names(table(survival$cause_source[survival$os_event == 1])),
                             table(survival$cause_source[survival$os_event == 1]), sep = " = ", collapse = "; "))

## ---- Data-quality flags ------------------------------------------------------------------
# Not corrected here (they need the study team), but listed so they are visible.

flags <- bind_rows(
  survival %>% filter(last_followup > DATA_CUT) %>%
    transmute(study_id, flag = "Last follow-up after the data cut", detail = paste("last_followup", last_followup)),
  survival %>% filter(cause == "RM", is.na(prog_date)) %>%
    transmute(study_id, flag = "Relapse-related death without progression date", detail = paste("death", death_date)),
  survival %>% filter(cause == "NRM", prog_before_death) %>%
    transmute(study_id, flag = "Non-relapse death after documented progression",
              detail = paste0("prog ", prog_date, ", death ", death_date, ", ", death_reason)),
  survival %>% filter(cause == "Unknown") %>%
    transmute(study_id, flag = "Death with unknown cause", detail = paste("death", death_date)),
  survival %>% filter(eos_reason == 2, is.na(prog_date)) %>%
    transmute(study_id, flag = "End of study for progression but no progression date", detail = paste("eos", eos_dov)),
  survival %>% filter(`5 year_survival` == 1 & os_event == 1 | `5 year_survival` == 0 & os_event == 0) %>%
    transmute(study_id, flag = "'5 year_survival' inconsistent with death date (not used)", detail = ""),
  survival %>% filter(pfs_event == 0, !is.na(eos_dov), last_followup - eos_dov > 180) %>%
    transmute(study_id, flag = "Progression-free & censored >6 mo after end of study (PFS ascertainment?)",
              detail = sprintf("%.1f months after EOS", as.numeric(last_followup - eos_dov) / DAYS_PER_MONTH)),
  rm %>% filter(visit_month == 0, actual_month > 0) %>%
    transmute(study_id, flag = "Baseline assessment after infusion",
              detail = sprintf("%.0f days after infusion", actual_month * DAYS_PER_MONTH)),
  rm %>% filter(visit_month > 0, abs(actual_month - visit_month) > 1.5) %>%
    transmute(study_id, flag = "Visit >1.5 months from its nominal time",
              detail = sprintf("%s visit at %.1f months", timepoint, actual_month))
) %>% arrange(flag, study_id)

message("\nData-quality flags (", nrow(flags), " rows):")
print(flags %>% count(flag), n = Inf)
save_table(flags, "data_quality_flags")

## ---- Analysis datasets ------------------------------------------------------------------

frailty_vars <- c("cfs_score", "grip_average_kg", "walk_time", "walk_mpers", "minicog_score",
                  "phq_total_score", "phq_10", "esas_wellbeing", "eortc_globalhealth",
                  "eortc_physicalfunction", "eq5d_vas")

baseline_frailty <- rm %>%
  filter(visit_month == 0) %>%
  select(study_id, all_of(frailty_vars), enrol_date, qol_required)

analysis_wide <- baseline %>%
  inner_join(baseline_frailty, by = "study_id") %>%
  inner_join(survival %>% select(-date_infusion), by = "study_id") %>%
  mutate(
    log2_ldh     = log2(ldh),
    ecog_cat     = factor(if_else(ecog >= 2, "2-3", "0-1"), levels = c("0-1", "2-3")),
    ves_cat      = factor(if_else(ves13_score >= 3, ">=3", "<3"), levels = c("<3", ">=3")),
    cfs_cat      = factor(if_else(cfs_score > 3, ">3", "1-3"), levels = c("1-3", ">3")),
    cfs_cat4     = factor(if_else(cfs_score > 4, ">4", "1-4"), levels = c("1-4", ">4")),
    gait_cat     = factor(if_else(walk_mpers > GAIT_SLOW_MPS, ">0.80", "<=0.80"), levels = c(">0.80", "<=0.80")),
    # sensitivity: an unmeasured gait test counted as slow
    gait_slow_nd = factor(if_else(is.na(walk_mpers) | walk_mpers <= GAIT_SLOW_MPS, "Slow or not done", ">0.80"),
                          levels = c(">0.80", "Slow or not done")),
    gait_done    = !is.na(walk_mpers),
    crs_any      = as.integer(crs_yn == "Yes"),
    crs_g2       = as.integer(crs_highestgrade >= 2),
    icans_any    = as.integer(icans_yn == "Yes"),
    icans_g2     = as.integer(icans_highest >= 2),
    icu          = as.integer(icu_yn == "Yes"),
    los_prolonged = as.integer(los_days > LOS_PROLONGED)
  )

analysis_long <- rm %>%
  left_join(survival %>% select(study_id, os_months, os_event, pfs_months, pfs_event, cause, cr_cause),
            by = "study_id")

stopifnot(
  "analysis_wide is not one row per patient" = nrow(analysis_wide) == nrow(baseline) && !anyDuplicated(analysis_wide$study_id),
  "a visit occurs after the end of OS follow-up" = all(analysis_long$visit_month <= analysis_long$os_months + 1e-6)
)

message(sprintf("\nanalysis_wide: %d patients x %d variables | analysis_long: %d visits",
                nrow(analysis_wide), ncol(analysis_wide), nrow(analysis_long)))

save_proc(baseline, "baseline")
save_proc(rm, "rm")
save_proc(survival, "survival")
save_proc(analysis_wide, "analysis_wide")
save_proc(analysis_long, "analysis_long")

message("02_clean_derive.R complete.")

