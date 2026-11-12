# ==============================================================================
# 11_check_abstract.R
#
# Purpose : Check that every number quoted in the abstract draft (EHA-EBMT CAR T
#           meeting submission, Nov 2026) is reproduced by this pipeline.
#           Compares output/v2/tables/key_results.csv (written by scripts
#           03-09) with the values typed in the abstract, after rounding to
#           the number of decimals shown in the abstract.
#           If the data change, re-run the pipeline and update the abstract
#           from the "computed" column of the output table.
# Inputs  : output/v2/tables/key_results.csv
# Outputs : output/v2/tables/abstract_check.csv
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "11_check_abstract.R"
section("11 - Check computed results against the abstract")

key <- read_csv(KEY_RESULTS_FILE, show_col_types = FALSE)

# what = which column to compare: value, lo or hi. digits = decimals shown in the abstract.
expected <- tribble(
  ~key,                          ~what,   ~abstract, ~digits, ~text,
  "n_patients",                  "value", 78,    0, "78 patients",
  "n_visits",                    "value", 233,   0, "233 assessment visits",
  "age_median",                  "value", 60,    0, "median age 60",
  "age_median",                  "lo",    22,    0, "range 22-83",
  "age_median",                  "hi",    83,    0, "range 22-83",
  "male_pct",                    "value", 59,    0, "59% male",
  "dlbcl_pct",                   "value", 47,    0, "DLBCL 47%",
  "tfl_pct",                     "value", 31,    0, "transformed FL 31%",
  "axicel_pct",                  "value", 85,    0, "85% axi-cel",
  "third_line_pct",              "value", 78,    0, "78% third-line",
  "bridged_pct",                 "value", 76,    0, "76% bridged",
  "feas_cfs_pct",                "value", 97,    0, "CFS 97%",
  "feas_phq9_pct",               "value", 99,    0, "PHQ-9 99%",
  "feas_esas_pct",               "value", 99,    0, "ESAS 99%",
  "feas_grip_pct",               "value", 93,    0, "grip strength 93%",
  "feas_minicog_pct",            "value", 91,    0, "Mini-Cog 91%",
  "feas_eq5d_pct",               "value", 97,    0, "questionnaires 97-99%",
  "feas_eortc_pct",              "value", 99,    0, "questionnaires 97-99%",
  "feas_gait_pct",               "value", 77,    0, "gait speed 77%",
  "gait_missing_mean_cfs",       "value", 3.7,   1, "mean CFS 3.7 (gait missed)",
  "gait_done_mean_cfs",          "value", 2.9,   1, "vs 2.9 (gait done)",
  "cfs_baseline_median",         "value", 3,     0, "baseline CFS median 3",
  "cfs_gt3_pct",                 "value", 40,    0, ">3 in 40%",
  "rho_baseline_cfs_ves13_score","value", 0.62,  2, "CFS vs VES-13 rho 0.62",
  "rho_baseline_cfs_walk_mpers", "value", -0.52, 2, "CFS vs gait speed -0.52",
  "rho_baseline_cfs_ecog",       "value", 0.50,  2, "CFS vs ECOG 0.50",
  "rho_baseline_cfs_grip_average_kg", "value", -0.44, 2, "CFS vs grip -0.44",
  "rho_post_cfs_gait_min",       "value", -0.60, 2, "post-infusion CFS vs gait -0.55 to -0.60",
  "rho_post_cfs_gait_max",       "value", -0.55, 2, "post-infusion CFS vs gait -0.55 to -0.60",
  "cfs_lsmean_baseline",         "value", 3.18,  2, "CFS 3.18 at baseline",
  "cfs_lsmean_1mo",              "value", 3.74,  2, "3.74 at 1 month",
  "cfs_lsmean_3mo",              "value", 2.86,  2, "2.86 at 3 months",
  "cfs_lsmean_12mo",             "value", 2.48,  2, "2.48 at 12 months",
  "crs_any_pct",                 "value", 87,    0, "CRS 87%",
  "crs_g2_pct",                  "value", 54,    0, "grade >=2 54%",
  "icans_any_pct",               "value", 28,    0, "ICANS 28%",
  "icu_pct",                     "value", 10,    0, "ICU 10%",
  "followup_median_months",      "value", 35.5,  1, "median follow-up 35.5 months",
  "deaths",                      "value", 34,    0, "34 deaths",
  "deaths_rm",                   "value", 23,    0, "23 RM",
  "deaths_nrm",                  "value", 11,    0, "11 NRM",
  "os_24m_pct",                  "value", 57,    0, "2-year OS 57%",
  "pfs_24m_pct",                 "value", 44,    0, "PFS 44%",
  "cif_rm_24m_pct",              "value", 29,    0, "2-year CIF RM 29%",
  "cif_nrm_24m_pct",             "value", 14,    0, "NRM 14%",
  "los_median",                  "value", 12,    0, "median 12 days",
  "los_rr_cfs_adj",              "value", 1.16,  2, "adjusted RR 1.16",
  "los_rr_cfs_adj",              "lo",    1.07,  2, "(1.07-1.26)",
  "los_rr_cfs_adj",              "hi",    1.26,  2, "(1.07-1.26)",
  "los_prolonged_pct_cfs_gt3",   "value", 45,    0, "stays >14 d in 45% with CFS >3",
  "los_prolonged_pct_cfs_1_3",   "value", 26,    0, "vs 26% with CFS 1-3",
  "os_hr_cfs_unadj",             "value", 1.32,  2, "OS HR 1.32 per point",
  "os_hr_cfs_unadj",             "lo",    1.08,  2, "(1.08-1.63)",
  "os_hr_cfs_unadj",             "hi",    1.63,  2, "(1.08-1.63)",
  "rm_shr_cfs_unadj",            "value", 1.46,  2, "RM sHR 1.46",
  "rm_shr_cfs_unadj",            "lo",    1.15,  2, "(1.15-1.84)",
  "rm_shr_cfs_unadj",            "hi",    1.84,  2, "(1.15-1.84)",
  "nrm_shr_cfs_unadj",           "value", 0.97,  2, "NRM sHR 0.97",
  "nrm_shr_cfs_unadj",           "lo",    0.71,  2, "(0.71-1.34)",
  "nrm_shr_cfs_unadj",           "hi",    1.34,  2, "(0.71-1.34)",
  "os_hr_cfs_adj",               "value", 1.17,  2, "adjusted OS HR 1.17",
  "os_hr_cfs_adj",               "lo",    0.92,  2, "(0.92-1.49)",
  # draft said 1.49 (double rounding of 1.485); exact value 1.4848 -> abstract corrected to 1.48
  "os_hr_cfs_adj",               "hi",    1.48,  2, "(0.92-1.48)",
  "rm_shr_cfs_adj",              "value", 1.20,  2, "adjusted RM sHR 1.20",
  "rm_shr_cfs_adj",              "lo",    0.90,  2, "(0.90-1.59)",
  "rm_shr_cfs_adj",              "hi",    1.59,  2, "(0.90-1.59)",
  "os_hr_gait_slow_or_nd",       "value", 2.06,  2, "slow/unmeasured gait HR 2.06",
  "os_hr_gait_slow_or_nd",       "lo",    1.05,  2, "(1.05-4.07)",
  "os_hr_gait_slow_or_nd",       "hi",    4.07,  2, "(1.05-4.07)",
  "os_hr_ecog23",                "value", 3.11,  2, "ECOG 2-3 HR 3.11",
  "tv_os_hr_cfs_adj",            "value", 1.41,  2, "time-varying CFS adjusted HR 1.41",
  "tv_os_hr_cfs_adj",            "lo",    1.11,  2, "(1.11-1.81)",
  "tv_os_hr_cfs_adj",            "hi",    1.81,  2, "(1.11-1.81)",
  "lm_n",                        "value", 71,    0, "71 patients alive at 3 months",
  "lm_traj_lower_pct",           "value", 62,    0, "lower-frailty group 62%",
  "lm_traj_higher_pct",          "value", 38,    0, "higher-frailty group 38%",
  "lm_os24_lower_pct",           "value", 68,    0, "2-year OS 68%",
  "lm_os24_higher_pct",          "value", 48,    0, "vs 48%",
  "lm_os_hr_traj",               "value", 2.23,  2, "HR 2.23",
  "lm_os_hr_traj",               "lo",    1.04,  2, "(1.04-4.77)",
  "lm_os_hr_traj",               "hi",    4.77,  2, "(1.04-4.77)",
  "lm_traj_nrm_pct_higher",      "value", 26,    0, "NRM 26%",
  "lm_traj_nrm_pct_lower",       "value", 9,     0, "vs 9% of patients",
  "lm_nrm_shr_traj",             "value", 2.82,  2, "sHR 2.82",
  "lm_nrm_shr_traj",             "lo",    0.83,  2, "(0.83-9.58)",
  "lm_nrm_shr_traj",             "hi",    9.58,  2, "(0.83-9.58)",
  "lm_os_hr_cfs3_adj",           "value", 1.47,  2, "CFS at 3 months adjusted HR 1.47",
  "lm_os_hr_cfs3_adj",           "lo",    1.09,  2, "(1.09-1.97)",
  "lm_os_hr_cfs3_adj",           "hi",    1.97,  2, "(1.09-1.97)",
  "lm_rm_shr_cfs3_adj",          "value", 1.36,  2, "RM sHR 1.36",
  "lm_rm_shr_cfs3_adj",          "lo",    0.98,  2, "(0.98-1.89)",
  "lm_rm_shr_cfs3_adj",          "hi",    1.89,  2, "(0.98-1.89)",
  "lm_nrm_shr_cfs3_adj",         "value", 1.51,  2, "NRM sHR 1.51",
  "lm_nrm_shr_cfs3_adj",         "lo",    0.95,  2, "(0.95-2.41)",
  "lm_nrm_shr_cfs3_adj",         "hi",    2.41,  2, "(0.95-2.41)"
)

check <- expected %>%
  left_join(key %>% select(key, value, lo, hi, p, note), by = "key") %>%
  mutate(
    computed = case_when(what == "value" ~ value, what == "lo" ~ lo, what == "hi" ~ hi),
    computed_rounded = round(computed, digits),
    status = case_when(is.na(computed) ~ "MISSING",
                       abs(computed_rounded - abstract) < 1e-9 ~ "OK",
                       TRUE ~ "DIFFERS")
  ) %>%
  select(status, key, what, abstract, computed_rounded, computed, text, note)

save_table(check, "abstract_check")

message(sprintf("Abstract numbers checked: %d | OK: %d | DIFFERS: %d | MISSING: %d",
                nrow(check), sum(check$status == "OK"), sum(check$status == "DIFFERS"), sum(check$status == "MISSING")))
if (any(check$status != "OK")) {
  message("\nNot matching:")
  print(check %>% filter(status != "OK") %>% select(status, text, abstract, computed_rounded, computed), n = Inf, width = Inf)
} else {
  message("All abstract numbers are reproduced by the pipeline.")
}

message("11_check_abstract.R complete.")
