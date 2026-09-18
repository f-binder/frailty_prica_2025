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

glimpse(rm_raw)


## ---- Timepoint -> numeric visit month -------------------------------------
# The assessment schedule is fixed (baseline, 1, 3, 6, 12 months); encoding
# it as a number rather than only a factor is what the trajectory/mixed
# models need visit_month for.

unique(rm_raw$timepoint)

visit_month_map <- c(
  "baseline"   = 0,
  "1 month"    = 1,
  "3 months"   = 3,
  "6 months"   = 6,
  "12 months"  = 12
)

unmapped <- setdiff(unique(rm_raw$timepoint), names(visit_month_map))
unmapped

if (length(unmapped) > 0) {
  stop(
    "Unrecognised timepoint value(s) not in visit_month_map: ",
    paste(unmapped, collapse = ", "),
    " - update visit_month_map before proceeding."
  )
}

rm(unmapped)

# explore IDs and number of timepoints per ID ------------------------
length(unique(rm_raw$study_id[!is.na(rm_raw$study_id)])) # 79 ids
setdiff(unique(rm_raw$study_id), baseline_clean$study_id) # - id 76 is still present here

rm_raw %>% filter(study_id == 76) %>%
  select(where(~any(!is.na(.)))) %>% glimpse() # only has baseline data



repeated_measures_clean <- 
rm_raw %>%
  mutate(
    study_id = as.integer(study_id),
    timepoint = factor(timepoint, levels = names(visit_month_map)),
    visit_month = recode(timepoint, !!!visit_month_map),
    visit_month = as.numeric(visit_month),
    dov = as.Date(dov)
  ) %>%
  # select(study_id, timepoint, visit_month, dov) # to check code
  # count(timepoint, visit_month) # checking modifications
  arrange(study_id, visit_month) %>%
  relocate(study_id, timepoint, visit_month, dov)

repeated_measures_clean

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

cfs_trajectory_plot <- 
cfs_by_visit %>%
  ggplot(aes(x = visit_month, y = mean)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.4, color = "steelblue") +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(size = 2.5, color = "steelblue") +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3.2) +
  scale_x_continuous(breaks = c(0, 1, 3, 6, 12)) +
  labs(
    title = "Mean CFS score over the first year post-CAR-T",
    subtitle = "Points: mean +/- SD; n declines over follow-up (death, dropout, not-yet-due)",
    x = "Months from CAR-T infusion",
    y = "CFS score"
  ) +
  theme_minimal(base_size = 12)

cfs_trajectory_plot

ggsave(
  here("output", "figures", "cfs_trajectory_mean_sd.png"),
  plot = cfs_trajectory_plot, width = 7, height = 5, dpi = 300
)

# same plot, only with the subset available at timepoint 12
cfs_by_visit_subset12 <- repeated_measures_clean %>%
  filter(!is.na(cfs_score) & visit_month == 12) %>%
  distinct(study_id) %>%
  left_join(repeated_measures_clean) %>%
  filter(!is.na(cfs_score)) %>%
  group_by(visit_month) %>%
  summarise(
    n = sum(!is.na(cfs_score)),
    mean = round(mean(cfs_score, na.rm = TRUE), 2),
    sd = round(sd(cfs_score, na.rm = TRUE), 2),
    .groups = "drop"
  )

cfs_by_visit_subset12

cfs_by_visit

cfs_by_visit_subset12 %>%
  ggplot(aes(x = visit_month, y = mean)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.4, color = "steelblue") +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(size = 2.5, color = "steelblue") +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3.2) +
  scale_x_continuous(breaks = c(0, 1, 3, 6, 12)) +
  labs(
    title = "Mean CFS score over the first year post-CAR-T",
    subtitle = "Points: mean +/- SD; only for subset who reached month 12 of follow-up",
    x = "Months from CAR-T infusion",
    y = "CFS score"
  ) +
  theme_minimal(base_size = 12)


## ---- Exploratory plot: individual CFS trajectories + population mean ----
   # including individual patient trajectories (spaghetti plot)

cfs_indiv <- repeated_measures_clean %>%
  filter(!is.na(cfs_score))

cfs_indiv

# cfs_trajectory_plot <- 
ggplot() +
  geom_line(
    data = cfs_indiv,
    aes(x = visit_month, y = cfs_score, group = study_id),
    color = "grey70", linewidth = 0.3, alpha = 0.5
  ) +
  geom_errorbar(
    data = cfs_by_visit,
    aes(x = visit_month, ymin = mean - sd, ymax = mean + sd),
    width = 0.4, linewidth = 0.6, color = "steelblue"
  ) +
  geom_line(
    data = cfs_by_visit,
    aes(x = visit_month, y = mean),
    color = "steelblue", linewidth = 1
  ) +
  geom_point(
    data = cfs_by_visit,
    aes(x = visit_month, y = mean),
    size = 2.5, color = "steelblue"
  ) +
  geom_text(
    data = cfs_by_visit,
    aes(x = visit_month, y = mean, label = paste0("n=", n)),
    vjust = -1.6, size = 3.2, color = "black"
  ) +
  scale_x_continuous(breaks = c(0, 1, 3, 6, 12)) +
  scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
  labs(
    title = "CFS trajectories over the first year post-CAR-T",
    subtitle = "Grey: individual patients. Blue: population mean +/- SD (n declines over follow-up)",
    x = "Months from CAR-T infusion",
    y = "Clinical Frailty Scale (CFS)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here("output", "figures", "cfs_trajectory_spaghetti.png"),
  plot = cfs_trajectory_plot, width = 7.5, height = 5.5, dpi = 300
)


message("\nRows: ", nrow(repeated_measures_clean), " | Patients: ", n_distinct(repeated_measures_clean$study_id))

## ---- Save ---------------------------------------------------------------

saveRDS(repeated_measures_clean, here("data", "processed", "repeated_measures_clean.rds"))

message("\n03_clean_repeated_measures.R complete. Wrote data/processed/repeated_measures_clean.rds")
