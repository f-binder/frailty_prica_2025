# ==============================================================================
# 04_feasibility.R
#
# Purpose : How feasible was each instrument? Two complementary measures:
#   (1) Completion: among ATTENDED visits, the % at which each test has a
#       result. Questionnaires that a protocol version did not require
#       (EORTC QLQ-C30, EQ-5D before 27 Apr 2022) are excluded from their
#       denominator, so protocol changes are not counted as non-feasibility.
#   (2) Attendance: attended visits / expected visits, where a visit is
#       expected if the patient was alive and under follow-up at that time.
#   Plus: is a missing gait test informative (frailer patients)?
# Inputs  : data/processed/v2/analysis_long.rds, analysis_wide.rds
# Outputs : output/v2/tables/feasibility_completion.csv,
#           feasibility_attendance.csv, feasibility_gait_missing.csv
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "04_feasibility.R"
section("04 - Feasibility of each instrument")

al <- read_proc("analysis_long")
d  <- read_proc("analysis_wide")

## ---- Instrument definitions ----------------------------------------------------------
# "Done" = the instrument's main score is recorded. Gait speed counts as done if
# either the time or the speed is recorded (they are always recorded together).

instruments <- tribble(
  ~instrument,      ~type,            ~done_expr,
  "CFS",            "Clinician",      "!is.na(cfs_score)",
  "Grip strength",  "Performance",    "!is.na(grip_average_kg)",
  "Gait speed",     "Performance",    "!is.na(walk_mpers) | !is.na(walk_time)",
  "Mini-Cog",       "Cognitive",      "!is.na(minicog_score)",
  "PHQ-9",          "Questionnaire",  "!is.na(phq_total_score)",
  "ESAS",           "Questionnaire",  "!is.na(esas_wellbeing)",
  "EORTC QLQ-C30",  "Questionnaire",  "!is.na(eortc_globalhealth)",
  "EQ-5D",          "Questionnaire",  "!is.na(eq5d_vas)"
)
protocol_restricted <- c("EORTC QLQ-C30", "EQ-5D")

long_done <- pmap_dfr(instruments, function(instrument, type, done_expr) {
  al %>%
    mutate(done = eval(parse(text = done_expr)),
           required = if (instrument %in% protocol_restricted) qol_required else TRUE) %>%
    transmute(study_id, visit, visit_month, instrument, type, done, required, cfs_score)
})

## ---- (1) Completion among attended visits --------------------------------------------

completion_by_visit <- long_done %>%
  filter(required) %>%
  group_by(instrument, type, visit) %>%
  summarise(visits = n(), done = sum(done), .groups = "drop") %>%
  mutate(pct_done = pct(done / visits, 1))

completion_overall <- long_done %>%
  filter(required) %>%
  group_by(instrument, type) %>%
  summarise(visits = n(), done = sum(done), .groups = "drop") %>%
  mutate(visit = "All visits", pct_done = pct(done / visits, 1))

# For comparison: the naive denominator (all attended visits, ignoring protocol)
completion_naive <- long_done %>%
  group_by(instrument) %>%
  summarise(pct_done_all_visits = pct(mean(done), 1), .groups = "drop")

completion <- bind_rows(completion_by_visit %>% mutate(visit = as.character(visit)), completion_overall) %>%
  mutate(visit = factor(visit, levels = c(VISIT_LABELS, "All visits")),
         instrument = factor(instrument, levels = instruments$instrument)) %>%
  arrange(instrument, visit)

completion_wide <- completion %>%
  select(instrument, visit, pct_done) %>%
  pivot_wider(names_from = visit, values_from = pct_done) %>%
  left_join(completion_naive, by = "instrument")
message("Completion (% of attended visits where the test was required):")
print(completion_wide, width = Inf)
save_table(completion, "feasibility_completion")
save_table(completion_wide, "feasibility_completion_wide")

# All five core frailty tests at the same visit
core <- c("CFS", "Grip strength", "Gait speed", "Mini-Cog", "PHQ-9")
all_core <- long_done %>%
  filter(instrument %in% core) %>%
  group_by(study_id, visit) %>%
  summarise(all_done = all(done), .groups = "drop") %>%
  group_by(visit) %>%
  summarise(visits = n(), pct_all_core = pct(mean(all_done), 1))
message("\nAll five core tests (CFS, grip, gait, Mini-Cog, PHQ-9) completed:")
print(all_core)
save_table(all_core, "feasibility_all_core_tests")

ov <- function(i) completion_overall$pct_done[completion_overall$instrument == i]
record("feas_cfs_pct", ov("CFS"))
record("feas_grip_pct", ov("Grip strength"))
record("feas_gait_pct", ov("Gait speed"))
record("feas_minicog_pct", ov("Mini-Cog"))
record("feas_phq9_pct", ov("PHQ-9"))
record("feas_esas_pct", ov("ESAS"))
record("feas_eortc_pct", ov("EORTC QLQ-C30"), note = "visits where protocol required it")
record("feas_eq5d_pct", ov("EQ-5D"), note = "visits where protocol required it")
record("feas_all_core_min_pct", min(all_core$pct_all_core), note = "lowest visit")
record("feas_all_core_max_pct", max(all_core$pct_all_core), note = "highest visit")

## ---- Is a missing gait test informative? ---------------------------------------------

gait_missing <- long_done %>%
  filter(instrument == "Gait speed") %>%
  group_by(gait_done = if_else(done, "Gait measured", "Gait not measured")) %>%
  summarise(visits = n(), mean_cfs = mean(cfs_score, na.rm = TRUE), sd_cfs = sd(cfs_score, na.rm = TRUE),
            cfs_ge5 = sum(cfs_score >= 5, na.rm = TRUE), .groups = "drop")
gm_test <- wilcox.test(cfs_score ~ done, data = long_done %>% filter(instrument == "Gait speed"), exact = FALSE)
gait_missing$p_wilcoxon <- gm_test$p.value
print(gait_missing)
save_table(gait_missing, "feasibility_gait_missing")
record("gait_missing_mean_cfs", gait_missing$mean_cfs[gait_missing$gait_done == "Gait not measured"], p = gm_test$p.value)
record("gait_done_mean_cfs", gait_missing$mean_cfs[gait_missing$gait_done == "Gait measured"])

# Same question at baseline only (one row per patient)
bl_gait <- d %>% group_by(gait_done) %>%
  summarise(n = n(), mean_cfs = mean(cfs_score), ecog_2_3 = sum(ecog >= 2, na.rm = TRUE), .groups = "drop")
message("\nBaseline: patients with vs without a gait speed result"); print(bl_gait)
record("gait_not_done_baseline_pct", pct(mean(!d$gait_done)))

## ---- (2) Attendance: attended / expected visits ------------------------------------------
# Expected: alive and under follow-up at the nominal visit time (os_months >= visit
# month). A stricter version also requires the patient to be still on study
# (end-of-study date on/after the nominal visit date).

surv <- read_proc("survival")
expected <- d %>%
  select(study_id, date_infusion, os_months) %>%
  left_join(surv %>% select(study_id, eos_dov), by = "study_id") %>%
  crossing(tibble(visit = factor(VISIT_LABELS, levels = VISIT_LABELS), visit_month = unname(VISITS))) %>%
  mutate(alive_followed = os_months >= visit_month,
         on_study = alive_followed & (is.na(eos_dov) | eos_dov >= date_infusion + visit_month * DAYS_PER_MONTH)) %>%
  left_join(al %>% distinct(study_id, visit_month) %>% mutate(attended = TRUE), by = c("study_id", "visit_month")) %>%
  mutate(attended = coalesce(attended, FALSE))

attendance <- expected %>%
  group_by(visit) %>%
  # computed before the counts below are named, so each formula uses the
  # per-patient columns rather than an already-summarised total
  summarise(attended_of_alive_pct = pct(sum(attended & alive_followed) / sum(alive_followed), 1),
            attended_of_on_study_pct = pct(sum(attended & on_study) / sum(on_study), 1),
            n_alive_followed = sum(alive_followed), n_on_study = sum(on_study),
            n_attended = sum(attended), .groups = "drop") %>%
  relocate(visit, n_alive_followed, n_on_study, n_attended)
message("\nVisit attendance:"); print(attendance)
save_table(attendance, "feasibility_attendance")

message("04_feasibility.R complete.")
