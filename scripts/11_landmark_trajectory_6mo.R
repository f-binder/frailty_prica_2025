# ==============================================================================
# 11_landmark_trajectory_6mo.R
#
# Purpose : Landmark analysis at 6 months post-infusion. Among patients alive
#           at the landmark, classify EARLY CFS trajectories using only
#           assessments made up to the landmark (baseline, 1, 3, 6 months),
#           then relate trajectory class to outcomes occurring AFTER the
#           landmark:
#             Part A - landmark cohort + visit-timing diagnostics
#             Part B - trajectory classification (group-based trajectory
#                      model, flexmix), with a transparent rule-based
#                      category as cross-check / fallback
#             Part C - raw event incidence by class
#             Part D - Cox models for OS and PFS from the landmark
#             Part E - competing risks: relapse-related (RM) vs non-relapse
#                      mortality (NRM) from the landmark
#
# Why a landmark design
# ---------------------
# Script 10 classified trajectories using data up to 12 months and measured
# survival from infusion. That is look-ahead (immortal-time) bias: patients
# who die early contribute only high peri-infusion CFS values and are pushed
# into the "elevated" class, while being classified "low" requires surviving
# long enough to be assessed repeatedly. Here, exposure (CFS trajectory) is
# fixed using information available at the landmark only, and outcome clocks
# start at the landmark, in the patients still at risk at that time.
#
# Inputs  : data/processed/analysis_wide.rds
#           data/processed/repeated_measures_clean.rds
# Outputs : output/tables/lm6_*.csv, output/figures/lm6_*.png,
#           output/models/lm6_*.rds
# Depends : 00_packages.R, ..., 05_merge_analysis_datasets.R
#
# Key analytic decisions (change here, not inline)
# ------------------------------------------------
# - LANDMARK = 6 months. Re-running with LANDMARK = 3 gives the 3-month
#   sensitivity analysis (all outputs are suffixed with the landmark).
# - Landmark cohort = alive and under follow-up at the landmark
#   (os_months >= LANDMARK). PFS analyses use the subset also free of
#   progression at the landmark.
# - Trajectory model: flexmix group-based trajectory model (same engine as
#   script 10), k = 1-3, linear vs quadratic in visit month; selected by
#   BIC. A solution is only accepted as "classifiable" if every class has
#   >= MIN_CLASS_N patients and >= MIN_CLASS_PROP of the cohort.
# - Rule-based category: CFS at the last assessment <= landmark vs baseline
#   (worse than baseline vs back to / better than baseline). Used as a
#   cross-check, and as the grouping variable if the model-based
#   classification fails.
# - Covariates are capped by events-per-variable (EPV = 10). Models that
#   exceed the cap are still fitted but flagged in the output table.
# - LDH enters as log2(LDH): HR per doubling (script 08 used raw LDH).
# - Cause of death from death_disease: 1 = relapse-related (RM),
#   0 = non-relapse (NRM), 2 / missing = unknown (separate competing cause).
# ==============================================================================

source(here::here("scripts", "00_packages.R"))
suppressPackageStartupMessages({
  library(flexmix)   # listed in 00_packages.R already; explicit for clarity
  library(cmprsk)
})

## ---- Parameters ------------------------------------------------------------

LANDMARK       <- 3 #6    # months post-infusion
MIN_CLASS_N    <- 5       # minimum patients per trajectory class
MIN_CLASS_PROP <- 0.10    # minimum proportion of the cohort per class
EPV            <- 10      # events per variable cap
N_REP          <- 20      # flexmix random restarts per candidate model
K_FORCE        <- NA      # NA = select k by BIC; set to 2 to force a 2-class solution
                          # (e.g. for direct comparability with script 10)
SEED           <- 20260923

tag <- paste0("lm", LANDMARK)   # file prefix, e.g. "lm6"

tables_dir  <- here("output", "tables")
figures_dir <- here("output", "figures")
models_dir  <- here("output", "models")
walk(c(tables_dir, figures_dir, models_dir), dir.create, showWarnings = FALSE, recursive = TRUE)

out_table  <- function(x, name) write_csv(x, file.path(tables_dir, paste0(tag, "_", name, ".csv")))
out_figure <- function(p, name, w = 8, h = 6) {
  png(file.path(figures_dir, paste0(tag, "_", name, ".png")), width = w, height = h, units = "in", res = 300)
  print(p)
  invisible(dev.off())
}
# Ordinal palette (low -> high frailty); passed explicitly everywhere because
# the project's plot_km() defaults to two colours only.
traj_palette <- function(n) switch(as.character(n),
  "1" = "#2C7BB6", "2" = c("#2C7BB6", "#D7191C"), "3" = c("#2C7BB6", "#F39C12", "#D7191C"))

section <- function(txt) message("\n", strrep("=", 78), "\n", txt, "\n", strrep("=", 78))

analysis_wide <- readRDS(here("data", "processed", "analysis_wide.rds"))
rm_clean      <- readRDS(here("data", "processed", "repeated_measures_clean.rds"))

## ============================================================================
## PART A - Landmark cohort
## ============================================================================
section(paste0("PART A - Landmark cohort (landmark = ", LANDMARK, " months)"))

d_all <- analysis_wide %>%
  mutate(
    study_id      = as.integer(study_id),
    cfs_baseline  = cfs_score,
    cart_line_num = readr::parse_number(as.character(cart_line)),
    log2_ldh      = log2(as.numeric(ldh)),
    death_cause   = case_when(
      os_event == 0                                ~ "alive",
      as.character(death_disease) == "1"           ~ "RM",
      as.character(death_disease) == "0"           ~ "NRM",
      TRUE                                         ~ "unknown"
    ),
    status_at_lm = case_when(
      os_months < LANDMARK & os_event == 1 ~ "Died before landmark",
      os_months < LANDMARK & os_event == 0 ~ "Censored before landmark",
      TRUE                                 ~ "Alive at landmark"
    ),
    # alive at the landmark, so a PFS event before it can only be progression
    prog_by_lm = as.integer(os_months >= LANDMARK & pfs_event == 1 & pfs_months < LANDMARK)
  )

if (any(is.na(d_all$cart_line_num) & !is.na(d_all$cart_line))) {
  warning("cart_line could not be parsed to a number for some patients - check its coding.")
}

cohort_flow <- bind_rows(
  tibble(step = "Infused cohort", n = nrow(d_all)),
  d_all %>% count(step = status_at_lm, name = "n"),
  tibble(step = "  of alive at landmark: progressed before landmark",
         n = sum(d_all$prog_by_lm)),
  tibble(step = "  of alive at landmark: progression-free at landmark (PFS cohort)",
         n = sum(d_all$status_at_lm == "Alive at landmark" & d_all$prog_by_lm == 0))
)
message("\n--- Cohort flow ---"); print(cohort_flow)
out_table(cohort_flow, "cohort_flow")
# 21 unavailable at landmark of 6 months (18 died and 3 were censored before landmark)

# Timing of the deaths that the landmark removes (answers: where did the
# NRM events of script 10 occur?)
early_deaths <- d_all %>%
  filter(os_event == 1) %>%
  mutate(period = if_else(os_months < LANDMARK, paste0("< ", LANDMARK, " mo"), paste0(">= ", LANDMARK, " mo"))) %>%
  count(period, death_cause) %>%
  pivot_wider(names_from = death_cause, values_from = n, values_fill = 0)

early_deaths %>% rowwise() %>%
  mutate(total = sum(c_across(-1)))

message("\n--- Deaths by cause, before vs after the landmark (whole cohort) ---"); print(early_deaths)

out_table(early_deaths, "deaths_by_cause_and_period")

lm_cohort <- d_all %>%
  filter(status_at_lm == "Alive at landmark") %>%
  mutate(
    os_lm  = os_months - LANDMARK,
    pfs_lm = pfs_months - LANDMARK   # only meaningful where prog_by_lm == 0
  )

lm_cohort

## ---- Exposure window: CFS assessments at or before the landmark ------------

traj_data <- rm_clean %>%
  mutate(study_id = as.integer(study_id)) %>%
  filter(study_id %in% lm_cohort$study_id, visit_month <= LANDMARK, !is.na(cfs_score)) %>%
  left_join(lm_cohort %>% select(study_id, date_infusion), by = "study_id") %>%
  mutate(actual_month = as.numeric(difftime(dov, date_infusion, units = "days")) / 30.4375)

traj_data

# Visit-timing diagnostic: the nominal landmark visit is sometimes performed
# after the landmark date. Using it is a small look-ahead (patient had to be
# alive at the visit). Reported so its size is visible, not assumed away.
timing_check <- traj_data %>%
  filter(visit_month == LANDMARK) %>%
  summarise(
    n_landmark_visits    = n(),
    n_missing_date       = sum(is.na(actual_month)),
    median_actual_month  = median(actual_month, na.rm = TRUE),
    max_actual_month     = max(actual_month, na.rm = TRUE),
    n_after_landmark     = sum(actual_month > LANDMARK, na.rm = TRUE),
    n_after_landmark_1mo = sum(actual_month > LANDMARK + 1, na.rm = TRUE)
  )
message("\n--- Timing of the nominal ", LANDMARK, "-month assessment ---"); print(as.data.frame(timing_check), digits = 3)
out_table(timing_check, "landmark_visit_timing")

obs_per_patient <- traj_data %>% count(study_id, name = "n_cfs_obs")
obs_per_patient
obs_per_patient %>% count(n_cfs_obs)

message("\n--- CFS assessments per patient within 0-", LANDMARK, " months (landmark cohort) ---")
print(table(obs_per_patient$n_cfs_obs))

# At least baseline + one post-infusion assessment needed to describe a
# trajectory; patients with a baseline value only are reported, not modelled.
classifiable_ids <- traj_data %>%
  group_by(study_id) %>%
  summarise(has_bl = any(visit_month == 0), n_post = sum(visit_month > 0), .groups = "drop") %>%
  filter(has_bl, n_post >= 1) %>%
  pull(study_id)

message("Classifiable (baseline + >= 1 post-infusion CFS): ", length(classifiable_ids),
        " of ", nrow(lm_cohort), " patients alive at the landmark")

traj_data <- traj_data %>% filter(study_id %in% classifiable_ids)

cfs_by_visit <- traj_data %>%
  group_by(visit_month) %>%
  summarise(n = n(), mean = mean(cfs_score), sd = sd(cfs_score), median = median(cfs_score), .groups = "drop")
message("\n--- CFS by visit (landmark cohort) ---"); print(cfs_by_visit)

## ============================================================================
## PART B - Trajectory classification (0 to landmark)
## ============================================================================
section("PART B - Early CFS trajectory classification")

set.seed(SEED)
traj_fit_data <- traj_data %>% mutate(study_id = factor(study_id))
n_pat <- n_distinct(traj_fit_data$study_id)

fit_k <- function(k, quadratic) {
  form <- if (quadratic) cfs_score ~ visit_month + I(visit_month^2) | study_id
          else           cfs_score ~ visit_month | study_id
  stepFlexmix(form, data = traj_fit_data, k = k, nrep = N_REP, verbose = FALSE,
              control = list(verbose = 0))
}

grid <- expand_grid(k = 1:3, quadratic = c(FALSE, TRUE))
grid # fits models for k of 1 to 3, either quadratic or not
fits <- map2(grid$k, grid$quadratic, fit_k)

model_comparison <- grid %>%
  mutate(
    form     = if_else(quadratic, "quadratic", "linear"),
    k_fitted = map_int(fits, ~ .x@k),
    logLik   = map_dbl(fits, ~ as.numeric(logLik(.x))),
    df       = map_dbl(fits, ~ attr(logLik(.x), "df")),
    BIC      = map_dbl(fits, BIC),                        # flexmix default (n = observations)
    BIC_subj = -2 * logLik + df * log(n_pat)              # n = patients (Nagin convention)
  ) %>%
  select(k_requested = k, form, k_fitted, logLik, df, BIC, BIC_subj)

message("\n--- Candidate models ---"); print(model_comparison)

best_idx <- which.min(model_comparison$BIC)
best_idx
if (!is.na(K_FORCE)) {
  forced <- which(model_comparison$k_fitted == K_FORCE)
  if (length(forced) == 0) stop("No candidate converged to k = ", K_FORCE)
  message("K_FORCE = ", K_FORCE, ": BIC-best model was k = ", model_comparison$k_fitted[best_idx],
          "; using the best k = ", K_FORCE, " model instead.")
  best_idx <- forced[which.min(model_comparison$BIC[forced])]
} else if (which.min(model_comparison$BIC_subj) != best_idx) {
  message("NOTE: observation-level and patient-level BIC select different models; ",
          "the observation-level BIC (as in script 10) is used. Report both.")
}
best_fit <- fits[[best_idx]]
best_fit
k_final  <- model_comparison$k_fitted[best_idx]
message("\nSelected: k = ", k_final, ", ", model_comparison$form[best_idx])

## ---- Patient-level posterior, class sizes, entropy -------------------------
# In a grouped flexmix model every observation of a patient shares the same
# posterior, so the first row per patient is the patient's posterior.

post <- posterior(best_fit)
post
patient_post <- traj_fit_data %>%
  select(study_id) %>%
  bind_cols(as_tibble(post, .name_repair = ~ paste0("p", seq_along(.x)))) %>%
  mutate(class_raw = clusters(best_fit)) %>%
  distinct(study_id, .keep_all = TRUE)

patient_post
patient_post %>% count(class_raw) %>% add_pcump()

p_mat <- as.matrix(select(patient_post, starts_with("p")))
entropy <- if (k_final > 1) {
  1 - sum(-p_mat * log(pmax(p_mat, 1e-12))) / (nrow(p_mat) * log(k_final))
} else NA_real_

# Class ordering: by fitted CFS at the landmark (lowest first)
coefs <- parameters(best_fit)
fitted_at <- function(t) {
  X <- if (nrow(coefs) >= 4) c(1, t, t^2) else c(1, t)   # rows: intercept, slope(s), sigma
  as.numeric(X %*% coefs[seq_along(X), , drop = FALSE])
}
class_order <- order(fitted_at(LANDMARK))

class_diag <- patient_post %>%
  mutate(assigned_prob = p_mat[cbind(seq_len(nrow(p_mat)), class_raw)]) %>%
  group_by(class_raw) %>%
  summarise(n = n(), mean_assigned_posterior = mean(assigned_prob), .groups = "drop") %>%
  mutate(
    prop = n / sum(n),
    fitted_cfs_baseline = fitted_at(0)[class_raw],
    fitted_cfs_1mo      = fitted_at(1)[class_raw],
    fitted_cfs_landmark = fitted_at(LANDMARK)[class_raw],
    order = match(class_raw, class_order)
  ) %>%
  arrange(order)

message("\n--- Class diagnostics ---"); print(as.data.frame(class_diag), digits = 3)
message("Normalised entropy: ", round(entropy, 3), " (>0.7 conventionally good)")

classifiable <- k_final >= 2 &&
  all(class_diag$n >= MIN_CLASS_N) &&
  all(class_diag$prop >= MIN_CLASS_PROP)

classifiable

if (classifiable) {
  message("\nRESULT: early trajectories ARE classifiable (k = ", k_final, ").")
} else {
  message("\nRESULT: early trajectories are NOT reliably classifiable ",
          "(best model k = ", k_final, ", or a class below the size threshold). ",
          "Parts C-E use the rule-based category instead.")
}

# Labels are ordinal (by fitted CFS at the landmark), not interpretive:
# the shape of each class is read from the figure/diagnostics, then named.
class_labels <- switch(as.character(k_final),
  "1" = "Single trajectory",
  "2" = c("Lower-frailty trajectory", "Higher-frailty trajectory"),
  "3" = c("Low-frailty trajectory", "Intermediate-frailty trajectory", "High-frailty trajectory")
)

patient_class <- patient_post %>%
  transmute(
    study_id = as.integer(as.character(study_id)),
    traj_class = factor(class_labels[match(class_raw, class_order)], levels = class_labels),
    assigned_posterior = p_mat[cbind(seq_len(nrow(p_mat)), class_raw)]
  )

patient_class

## ---- Rule-based category (cross-check / fallback) ---------------------------

cfs_summary <- traj_data %>%
  group_by(study_id) %>%
  summarise(
    cfs_bl        = cfs_score[visit_month == 0][1],
    cfs_peak_post = max(cfs_score[visit_month > 0]),
    cfs_last      = cfs_score[which.max(visit_month)],       # last CFS <= landmark
    last_visit    = max(visit_month),
    .groups = "drop"
  ) %>%
  mutate(
    cfs_change = cfs_last - cfs_bl,
    rule_cat = factor(if_else(cfs_change > 0, "Worse than baseline", "Back to / better than baseline"),
                      levels = c("Back to / better than baseline", "Worse than baseline"))
  )

message("\n--- Last CFS used (visit month of last assessment <= landmark) ---")
print(table(cfs_summary$last_visit))

lm_data <- lm_cohort %>%
  inner_join(patient_class, by = "study_id") %>%
  inner_join(cfs_summary, by = "study_id")

if (classifiable) {
  lm_data <- lm_data %>% mutate(traj = traj_class)
  grouping_used <- "model-based trajectory class"
} else {
  lm_data <- lm_data %>% mutate(traj = rule_cat)
  grouping_used <- "rule-based CFS change category"
}
message("\nGrouping used downstream: ", grouping_used)

## ---- Is the class just baseline (or landmark) CFS in disguise? --------------

xt_baseline <- lm_data %>%
  mutate(baseline_cfs_gt3 = if_else(cfs_bl > 3, "Baseline CFS >3", "Baseline CFS 1-3")) %>%
  count(traj, baseline_cfs_gt3) %>%
  pivot_wider(names_from = baseline_cfs_gt3, values_from = n, values_fill = 0)
xt_rule <- lm_data %>% count(traj, rule_cat) %>%
  pivot_wider(names_from = rule_cat, values_from = n, values_fill = 0)

message("\n--- Trajectory group vs baseline CFS >3 ---"); print(as.data.frame(xt_baseline))
message("\n--- Trajectory group vs rule-based category ---"); print(as.data.frame(xt_rule))

class_profile <- lm_data %>%
  group_by(traj) %>%
  summarise(
    n = n(),
    age_median = median(age, na.rm = TRUE),
    ldh_median = median(as.numeric(ldh), na.rm = TRUE),
    cart_line_4plus = sum(cart_line_num >= 4, na.rm = TRUE),
    cfs_bl_mean = mean(cfs_bl), cfs_peak_mean = mean(cfs_peak_post), cfs_last_mean = mean(cfs_last),
    cfs_change_mean = mean(cfs_change),
    worse_than_baseline = sum(rule_cat == "Worse than baseline"),
    progressed_before_lm = sum(prog_by_lm),
    .groups = "drop"
  )
message("\n--- Profile by trajectory group ---"); print(as.data.frame(class_profile), digits = 3)

out_table(model_comparison, "traj_model_comparison")
out_table(class_diag %>% mutate(entropy = entropy, classifiable = classifiable), "traj_class_diagnostics")
out_table(class_profile, "traj_class_profile")
out_table(xt_baseline, "traj_vs_baseline_cfs")
saveRDS(best_fit, file.path(models_dir, paste0(tag, "_traj_flexmix.rds")))
saveRDS(lm_data, here("data", "processed", paste0(tag, "_landmark_data.rds")))

## ---- Trajectory figure: individual CFS paths + class-fitted means ----------

grid_t <- seq(0, LANDMARK, by = 0.25)
fitted_lines <- map_dfr(seq_len(k_final), function(j) {
  tibble(visit_month = grid_t,
         cfs = map_dbl(grid_t, ~ fitted_at(.x)[j]),
         traj_class = class_labels[match(j, class_order)])
})

traj_plot <- traj_data %>%
  inner_join(lm_data %>% select(study_id, traj_class), by = "study_id") %>%
  ggplot(aes(visit_month, cfs_score, colour = traj_class)) +
  geom_line(aes(group = study_id), alpha = 0.25,
            position = position_jitter(width = 0, height = 0.08, seed = 1)) +
  geom_smooth(linetype = "dashed") +
  # geom_line(data = fitted_lines, aes(y = cfs), linewidth = 1.4) +
  scale_x_continuous(breaks = sort(unique(traj_data$visit_month))) +
  scale_y_continuous(breaks = 1:9) +
  scale_colour_manual(values = traj_palette(k_final)) +
  labs(x = "Months from CAR T infusion", y = "Clinical Frailty Scale",
       colour = NULL,
       title = paste0("Early CFS trajectories (0-", LANDMARK, " months), patients alive at ", LANDMARK, " months"),
       subtitle = paste0("Thin lines: individual patients;",
                         #" thick lines: class-fitted means (n = ",
                         " thick lines: class means (n = ",
                         nrow(lm_data), ")")) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")

traj_plot

out_figure(traj_plot, "traj_classes", w = 8, h = 5.5)

## ============================================================================
## PART C - Raw incidence of post-landmark events by trajectory group
## ============================================================================
section("PART C - Raw incidence of events after the landmark")

pfs_data <- lm_data %>% filter(prog_by_lm == 0)

rate_row <- function(events, pm) {
  ci <- poisson.test(events, pm / 12)$conf.int * 100
  tibble(events = events, person_years = pm / 12,
         rate_per_100py = 100 * events / (pm / 12), rate_low = ci[1], rate_high = ci[2])
}

incidence <- bind_rows(
  lm_data %>% group_by(traj) %>%
    summarise(outcome = "Death (any cause)", n_at_risk = n(),
              r = list(rate_row(sum(os_event), sum(os_lm))), .groups = "drop"),
  lm_data %>% group_by(traj) %>%
    summarise(outcome = "Relapse-related death (RM)", n_at_risk = n(),
              r = list(rate_row(sum(death_cause == "RM"), sum(os_lm))), .groups = "drop"),
  lm_data %>% group_by(traj) %>%
    summarise(outcome = "Non-relapse death (NRM)", n_at_risk = n(),
              r = list(rate_row(sum(death_cause == "NRM"), sum(os_lm))), .groups = "drop"),
  lm_data %>% group_by(traj) %>%
    summarise(outcome = "Death, unknown cause", n_at_risk = n(),
              r = list(rate_row(sum(death_cause == "unknown"), sum(os_lm))), .groups = "drop"),
  pfs_data %>% group_by(traj) %>%
    summarise(outcome = "Progression or death (PFS event; progression-free at landmark)",
              n_at_risk = n(), r = list(rate_row(sum(pfs_event), sum(pfs_lm))), .groups = "drop")
) %>%
  unnest(r) %>%
  mutate(pct_with_event = 100 * events / n_at_risk) %>%
  relocate(outcome, traj, n_at_risk, events, pct_with_event) %>%
  arrange(outcome, traj)

message("\n--- Events after the landmark, by trajectory group ---")
print(as.data.frame(incidence %>% mutate(across(where(is.double), ~ round(.x, 1)))))

km_os_lm  <- survfit(Surv(os_lm, os_event) ~ traj, data = lm_data)
km_pfs_lm <- survfit(Surv(pfs_lm, pfs_event) ~ traj, data = pfs_data)
km_timepoints <- bind_rows(
  summary(km_os_lm, times = c(12, 24), extend = TRUE) %>%
    { tibble(outcome = "OS", traj = str_remove(.$strata, "^traj="), months_after_lm = .$time,
             n_risk = .$n.risk, estimate = .$surv, lower = .$lower, upper = .$upper) },
  summary(km_pfs_lm, times = c(12, 24), extend = TRUE) %>%
    { tibble(outcome = "PFS", traj = str_remove(.$strata, "^traj="), months_after_lm = .$time,
             n_risk = .$n.risk, estimate = .$surv, lower = .$lower, upper = .$upper) }
)
message("\n--- KM estimates 12 and 24 months after the landmark ---")
print(as.data.frame(km_timepoints %>% mutate(across(c(estimate, lower, upper), ~ round(.x, 2)))))

out_table(incidence, "incidence_by_traj")
out_table(km_timepoints, "km_timepoints_by_traj")

## ---- KM figures ---------------------------------------------------------------
# Uses plot_km() from the project (script 08 / 00_packages.R) if loaded;
# otherwise a minimal local fallback so this script stays self-contained.
library(patchwork)
if (!exists("plot_km")) {
  plot_km <- function(fit, formula, data, xlab = "Time", ylab = "Survival probability",
                      legend_title = NULL, palette = traj_palette(2),
                      risk_table = TRUE) {
    km_df <- broom::tidy(fit) %>% mutate(strata = str_remove(strata, "^[^=]+="))
    km_df <- bind_rows(km_df %>% distinct(strata) %>%
                         mutate(time = 0, estimate = 1, conf.low = 1, conf.high = 1, n.censor = 0),
                       km_df) %>% arrange(strata, time)
    p <- survdiff(formula, data = data)$pvalue
    p_label <- if (p < 0.001) "Log-rank p < 0.001" else paste0("Log-rank p = ", sprintf("%.3f", p))
    curve <- ggplot(km_df, aes(time, estimate, colour = strata)) +
      geom_step(linewidth = 0.9) +
      geom_point(data = filter(km_df, n.censor > 0, time > 0), shape = 3, size = 2, show.legend = FALSE) +
      annotate("text", x = 0, y = 0.05, label = p_label, hjust = 0, size = 4) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      scale_colour_manual(values = palette) +
      labs(x = xlab, y = ylab, colour = legend_title) +
      theme_minimal(base_size = 12) + theme(legend.position = "top")
    if (!risk_table) return(curve)
    br <- scales::extended_breaks()(c(0, max(km_df$time)))
    br <- br[br >= 0 & br <= max(km_df$time)]
    risk_df <- map_dfr(br, function(t) {
      s <- summary(fit, times = t, extend = TRUE)
      tibble(time = t, strata = str_remove(as.character(s$strata), "^[^=]+="), n.risk = s$n.risk)
    })
    tab <- ggplot(risk_df, aes(time, strata, label = n.risk)) + geom_text(size = 3.5) +
      scale_x_continuous(limits = range(km_df$time), breaks = br) + labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank())
    curve / tab + plot_layout(heights = c(4, 1))
  }
}


plot_km(km_os_lm, Surv(os_lm, os_event) ~ traj, data = lm_data,
        xlab = paste0("Months after ", LANDMARK, "-month landmark"),
        ylab = "Overall survival", legend_title = NULL,
        palette = traj_palette(nlevels(lm_data$traj)))

plot_km(km_pfs_lm, Surv(pfs_lm, pfs_event) ~ traj, data = pfs_data,
        xlab = paste0("Months after ", LANDMARK, "-month landmark"),
        ylab = "Progression-free survival", legend_title = NULL,
        palette = traj_palette(nlevels(lm_data$traj)))


out_figure(plot_km(km_os_lm, Surv(os_lm, os_event) ~ traj, data = lm_data,
                   xlab = paste0("Months after ", LANDMARK, "-month landmark"),
                   ylab = "Overall survival", legend_title = NULL,
                   palette = traj_palette(nlevels(lm_data$traj))),
           "km_os_by_traj", w = 7.5, h = 6.5)
out_figure(plot_km(km_pfs_lm, Surv(pfs_lm, pfs_event) ~ traj, data = pfs_data,
                   xlab = paste0("Months after ", LANDMARK, "-month landmark"),
                   ylab = "Progression-free survival", legend_title = NULL,
                   palette = traj_palette(nlevels(lm_data$traj))),
           "km_pfs_by_traj", w = 7.5, h = 6.5)

## ============================================================================
## PART D - Cox models from the landmark (OS, PFS)
## ============================================================================
section("PART D - Cox models from the landmark")

#' Fit one Cox model and return a tidy row set with model-level diagnostics:
#' N, events, EPV status, PH test, concordance, and a separation flag
#' (a group with zero events makes its HR non-estimable).
fit_cox <- function(data, time, event, rhs, label, outcome) {
  f <- as.formula(paste0("Surv(", time, ", ", event, ") ~ ", rhs))
  n_terms <- ncol(model.matrix(f, data = data)) - 1
  warns <- character()
  fit <- withCallingHandlers(
    coxph(f, data = data),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  flag <- if (length(warns)) paste(unique(warns), collapse = "; ") else NA_character_
  zph <- tryCatch(cox.zph(fit)$table["GLOBAL", "p"], error = function(e) NA_real_)
  tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      outcome = outcome, model = label, n = fit$n, events = fit$nevent,
      n_terms = n_terms, epv = fit$nevent / n_terms,
      epv_ok = fit$nevent / n_terms >= EPV,
      concordance = unname(fit$concordance["concordance"]),
      ph_global_p = zph, aic = AIC(fit), fit_warning = flag
    ) %>%
    relocate(outcome, model)
}

events_by_group <- function(data, event) data %>% group_by(traj) %>% summarise(e = sum(.data[[event]])) %>% pull(e)

message("OS events after landmark by group: ",
        paste(levels(lm_data$traj), events_by_group(lm_data, "os_event"), sep = " = ", collapse = "; "))
message("PFS events after landmark by group: ",
        paste(levels(pfs_data$traj), events_by_group(pfs_data, "pfs_event"), sep = " = ", collapse = "; "))

message("EPV cap: OS supports ", floor(sum(lm_data$os_event) / EPV), " term(s); PFS supports ",
        floor(sum(pfs_data$pfs_event) / EPV), " term(s). Models above the cap are flagged (epv_ok = FALSE).")

cox_specs <- tribble(
  ~model,                                      ~rhs,
  "M1 trajectory (unadjusted)",                "traj",
  "M2 trajectory + log2 LDH (primary)",        "traj + log2_ldh",
  "M3 trajectory + baseline CFS",              "traj + cfs_bl",
  "M4 trajectory + age + log2 LDH + line",     "traj + age + log2_ldh + cart_line_num",
  "L1 baseline CFS only",                      "cfs_bl",
  "L2 CFS at landmark + baseline CFS",         "cfs_last + cfs_bl"
)

cox_os <- pmap_dfr(cox_specs, function(model, rhs)
  fit_cox(lm_data, "os_lm", "os_event", rhs, model, "OS"))
# OS-specific sensitivity: account for progression before the landmark
cox_os <- bind_rows(
  cox_os,
  fit_cox(lm_data, "os_lm", "os_event", "traj + log2_ldh + prog_by_lm",
          "S1 M2 + progressed before landmark", "OS"),
  fit_cox(filter(lm_data, prog_by_lm == 0), "os_lm", "os_event", "traj + log2_ldh",
          "S2 M2 restricted to progression-free at landmark", "OS")
)
cox_pfs <- pmap_dfr(cox_specs, function(model, rhs)
  fit_cox(pfs_data, "pfs_lm", "pfs_event", rhs, model, "PFS"))

cox_results <- bind_rows(cox_os, cox_pfs)

message("\n--- Cox results (HR, 95% CI) ---")
cox_results %>%
  transmute(outcome, model, term, HR = round(estimate, 2), low = round(conf.low, 2),
            high = round(conf.high, 2), p = signif(p.value, 2), n, events,
            epv_ok, C = round(concordance, 3), ph_p = round(ph_global_p, 3)) %>%
  as.data.frame() %>% print()

if (any(!is.na(cox_results$fit_warning))) {
  message("\nFit warnings (typically zero events in a group -> non-estimable HR):")
  cox_results %>% filter(!is.na(fit_warning)) %>% distinct(outcome, model, fit_warning) %>% print(width = Inf)
}

out_table(cox_results, "cox_os_pfs")

## ============================================================================
## PART E - Competing risks from the landmark: RM vs NRM
## ============================================================================
section("PART E - Competing risks from the landmark (RM vs NRM)")

cr_data <- lm_data %>%
  mutate(cr_status = case_when(
    death_cause == "alive"   ~ 0L,
    death_cause == "RM"      ~ 1L,
    death_cause == "NRM"     ~ 2L,
    death_cause == "unknown" ~ 3L
  ))

cr_counts <- cr_data %>%
  mutate(cr_label = factor(cr_status, levels = 0:3,
                           labels = c("Alive/censored", "RM", "NRM", "Unknown cause"))) %>%
  count(traj, cr_label) %>%
  pivot_wider(names_from = cr_label, values_from = n, values_fill = 0)
message("\n--- Post-landmark deaths by cause and trajectory group ---"); print(as.data.frame(cr_counts))
out_table(cr_counts, "cr_counts_by_traj")

ci_fit <- cuminc(ftime = cr_data$os_lm, fstatus = cr_data$cr_status, group = cr_data$traj, cencode = 0)

gray_tests <- as_tibble(ci_fit$Tests, rownames = "cause") %>%
  mutate(cause = recode(cause, "1" = "RM", "2" = "NRM", "3" = "Unknown cause"))
message("\n--- Gray's test (difference in cumulative incidence between groups) ---"); print(gray_tests)

cif_at <- timepoints(ci_fit, times = c(12, 24))
cif_table <- as_tibble(cif_at$est, rownames = "group_cause") %>%
  pivot_longer(-group_cause, names_to = "months_after_lm", values_to = "cif") %>%
  mutate(cause_code = str_extract(group_cause, "\\d+$"),
         traj = factor(str_remove(group_cause, " \\d+$"), levels = levels(lm_data$traj)),
         cause = recode(cause_code, "1" = "RM", "2" = "NRM", "3" = "Unknown cause")) %>%
  select(cause, traj, months_after_lm, cif)
message("\n--- Cumulative incidence at 12 and 24 months after landmark ---")
print(as.data.frame(cif_table %>% mutate(cif = round(cif, 3))))

## ---- Regression: Fine-Gray and cause-specific Cox, per cause ----------------
# Only fitted when every group has >= 1 event for that cause (otherwise the
# HR is non-estimable and the result is reported descriptively). NRM and RM
# event counts are expected to be far below the EPV threshold for any
# adjustment, so these are univariable (trajectory group only).

traj_mm <- model.matrix(~ traj, cr_data)[, -1, drop = FALSE]

cr_regression <- map_dfr(c(RM = 1L, NRM = 2L), function(code) {
  cause <- c("1" = "RM", "2" = "NRM")[as.character(code)]
  ev_by_group <- tapply(cr_data$cr_status == code, cr_data$traj, sum)
  if (any(ev_by_group == 0) || sum(ev_by_group) < 3) {
    message(cause, ": events by group = ", paste(names(ev_by_group), ev_by_group, sep = " ", collapse = "; "),
            " -> regression not estimable; report counts, CIF and Gray's test only.")
    return(tibble(cause = cause, method = c("Fine-Gray", "Cause-specific Cox"),
                  term = NA_character_, HR = NA_real_, low = NA_real_, high = NA_real_,
                  p = NA_real_, events = sum(ev_by_group), note = "Not estimable (zero events in a group)"))
  }
  fg <- crr(ftime = cr_data$os_lm, fstatus = cr_data$cr_status, cov1 = traj_mm,
            failcode = code, cencode = 0)
  fg_s <- summary(fg)$coef
  cs <- coxph(Surv(os_lm, cr_status == code) ~ traj, data = cr_data)
  bind_rows(
    tibble(cause = cause, method = "Fine-Gray (subdistribution HR)", term = rownames(fg_s),
           HR = exp(fg_s[, "coef"]),
           low = exp(fg_s[, "coef"] - 1.96 * fg_s[, "se(coef)"]),
           high = exp(fg_s[, "coef"] + 1.96 * fg_s[, "se(coef)"]),
           p = fg_s[, "p-value"], events = sum(ev_by_group),
           note = if (fg$converged) NA_character_ else "Did not converge"),
    tidy(cs, exponentiate = TRUE, conf.int = TRUE) %>%
      transmute(cause = cause, method = "Cause-specific Cox (HR)", term, HR = estimate,
                low = conf.low, high = conf.high, p = p.value, events = cs$nevent, note = NA_character_)
  )
})

message("\n--- Competing-risk regression (trajectory group; univariable) ---")
print(as.data.frame(cr_regression %>% mutate(term = str_remove(term, "^traj"), across(c(HR, low, high), ~ round(.x, 2)), p = signif(p, 2))))

out_table(gray_tests, "cr_gray_tests")
out_table(cif_table, "cr_cif_timepoints")
out_table(cr_regression, "cr_regression")

## ---- CIF figure -------------------------------------------------------------------

cif_df <- map_dfr(setdiff(names(ci_fit), "Tests"), function(nm) {
  tibble(group_cause = nm, time = ci_fit[[nm]]$time, est = ci_fit[[nm]]$est)
}) %>%
  mutate(cause_code = str_extract(group_cause, "\\d+$"),
         traj = factor(str_remove(group_cause, " \\d+$"), levels = levels(lm_data$traj))) %>%
  filter(cause_code %in% c("1", "2")) %>%
  left_join(gray_tests %>% mutate(cause_code = c(RM = "1", NRM = "2", `Unknown cause` = "3")[cause]) %>%
              select(cause_code, pv), by = "cause_code") %>%
  mutate(panel = paste0(if_else(cause_code == "1", "Relapse-related death", "Non-relapse death"),
                        " (Gray's p = ", sprintf("%.3f", pv), ")"),
         panel = fct_reorder(panel, as.integer(cause_code)))

cif_plot <- ggplot(cif_df, aes(time, est, colour = traj)) +
  geom_step(linewidth = 0.9) +
  facet_wrap(~ panel) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_colour_manual(values = traj_palette(nlevels(lm_data$traj))) +
  labs(x = paste0("Months after ", LANDMARK, "-month landmark"), y = "Cumulative incidence", colour = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
cif_plot
out_figure(cif_plot, "cif_rm_nrm_by_traj", w = 9, h = 5)

## ---- Sensitivity: last CFS at/before the landmark vs trajectory class ---------
# Question: does a single CFS value - the last assessment at or before the
# landmark (6-month value, or the 3-month value carried forward if the
# 6-month one is missing) - predict RM/NRM as well as the trajectory class?
# If it does, the simpler measure is preferable clinically.
#
# Models per cause (Fine-Gray, subdistribution HR), same patients throughout:
#   F1  trajectory class              (as above, repeated for side-by-side)
#   F2  last CFS, continuous          (sHR per 1-point increase)
#   F3  last CFS > 3 vs 1-3           (binary, mirrors the usual CFS cut-off)
#   F4  trajectory class + last CFS   (does either retain an effect given the other?)
#       Class and last CFS are strongly correlated by construction, and RM/NRM
#       event counts are small: F4 is over-parameterised relative to the EPV
#       rule and its estimates will be unstable. Read it as a check, not a result.
# Comparison metric: AIC from the Fine-Gray log pseudo-likelihood. Models are
# fitted on identical data, so AIC is comparable, but it is a pseudo-likelihood
# and should be read as a heuristic ranking, not a formal test.
# A continuous predictor stays estimable when a class has zero events for a
# cause (the NRM situation in script 10) - one practical advantage of F2.

message("\n--- Sensitivity: last CFS <= landmark vs trajectory class (Fine-Gray) ---")
message("Last CFS taken from month: ",
        paste(names(table(cr_data$last_visit)), table(cr_data$last_visit), sep = " = n ", collapse = "; "))

cr_data <- cr_data %>%
  mutate(cfs_last_gt3 = factor(if_else(cfs_last > 3, "CFS >3", "CFS 1-3"), levels = c("CFS 1-3", "CFS >3")))

fg_specs <- tribble(
  ~model,                                 ~rhs,
  "F1 trajectory class",                  "traj",
  "F2 last CFS (continuous, per point)",  "cfs_last",
  "F3 last CFS >3 vs 1-3",                "cfs_last_gt3",
  "F4 trajectory class + last CFS",       "traj + cfs_last"
)

#' Fine-Gray fit returning tidy rows + AIC. If any level of a categorical
#' predictor (including the reference level) has zero events for the cause, the
#' sHR is non-estimable: the model is skipped and reported as such. Fits that do
#' not converge have their estimates blanked - crr() otherwise returns absurd
#' sHRs (e.g. >10,000) in exactly this sparse-event situation.
fit_fg <- function(rhs, model, code) {
  cause <- c("1" = "RM", "2" = "NRM")[as.character(code)]
  X <- model.matrix(as.formula(paste("~", rhs)), cr_data)[, -1, drop = FALSE]
  is_event <- cr_data$cr_status == code
  
  empty <- unlist(map(all.vars(as.formula(paste("~", rhs))), function(v) {
    x <- cr_data[[v]]
    if (!(is.factor(x) || is.character(x))) return(NULL)
    ev <- tapply(is_event, x, sum)
    names(ev)[ev == 0]
  }))
  if (length(empty) > 0) {
    return(tibble(cause = cause, model = model, term = NA_character_, sHR = NA_real_,
                  low = NA_real_, high = NA_real_, p = NA_real_, events = sum(is_event),
                  aic = NA_real_, note = paste("Not estimable: zero events in", paste(empty, collapse = ", "))))
  }
  
  fg <- crr(ftime = cr_data$os_lm, fstatus = cr_data$cr_status, cov1 = X, failcode = code, cencode = 0)
  s <- summary(fg)$coef
  tibble(
    cause = cause, model = model, term = rownames(s),
    sHR  = exp(s[, "coef"]),
    low  = exp(s[, "coef"] - 1.96 * s[, "se(coef)"]),
    high = exp(s[, "coef"] + 1.96 * s[, "se(coef)"]),
    p = s[, "p-value"], events = sum(is_event),
    aic = -2 * fg$loglik + 2 * length(fg$coef),
    note = NA_character_
  ) %>%
    { if (fg$converged) . else mutate(., across(c(sHR, low, high, p, aic), ~ NA_real_),
                                      note = "Did not converge - estimates suppressed") }
}

cr_sens_last_cfs <- map_dfr(c(1L, 2L), function(code)
  pmap_dfr(fg_specs, function(model, rhs) fit_fg(rhs, model, code))) %>%
  mutate(term = str_remove(term, "^traj|^cfs_last_gt3"))

print(as.data.frame(cr_sens_last_cfs %>%
                      mutate(across(c(sHR, low, high), ~ round(.x, 2)), p = signif(p, 2), aic = round(aic, 1))))

# Descriptive parity with the trajectory analysis: counts and Gray's test by
# last CFS category
cr_counts_last_cfs <- cr_data %>%
  mutate(cr_label = factor(cr_status, levels = 0:3,
                           labels = c("Alive/censored", "RM", "NRM", "Unknown cause"))) %>%
  count(cfs_last_gt3, cr_label) %>%
  pivot_wider(names_from = cr_label, values_from = n, values_fill = 0)

ci_last <- cuminc(ftime = cr_data$os_lm, fstatus = cr_data$cr_status, group = cr_data$cfs_last_gt3, cencode = 0)
gray_last <- as_tibble(ci_last$Tests, rownames = "cause") %>%
  mutate(cause = recode(cause, "1" = "RM", "2" = "NRM", "3" = "Unknown cause"))

message("\n--- Post-landmark deaths by cause, by last CFS category ---"); print(as.data.frame(cr_counts_last_cfs))
message("\n--- Gray's test by last CFS category ---"); print(as.data.frame(gray_last), digits = 3)

# Lower AIC = better fit; differences < ~2 are not meaningful
aic_compare <- cr_sens_last_cfs %>%
  distinct(cause, model, aic) %>%
  group_by(cause) %>%
  mutate(delta_aic = aic - min(aic, na.rm = TRUE)) %>%
  ungroup()
message("\n--- AIC comparison (per cause; lowest = 0) ---"); print(as.data.frame(aic_compare), digits = 3)

out_table(cr_sens_last_cfs, "cr_sensitivity_last_cfs")
out_table(cr_counts_last_cfs, "cr_counts_by_last_cfs")
out_table(gray_last, "cr_gray_tests_last_cfs")
out_table(aic_compare, "cr_aic_traj_vs_last_cfs")

## ============================================================================
## Summary
## ============================================================================
section("SUMMARY")
message(
  "Landmark: ", LANDMARK, " months | alive at landmark: ", nrow(lm_cohort),
  " | classified: ", nrow(lm_data), " | PFS cohort: ", nrow(pfs_data), "\n",
  "Trajectory model: k = ", k_final, " (", model_comparison$form[best_idx], "), entropy = ",
  round(entropy, 2), ", classifiable = ", classifiable, " -> grouping used: ", grouping_used, "\n",
  "Post-landmark events: deaths = ", sum(lm_data$os_event), " (RM ", sum(lm_data$death_cause == "RM"),
  ", NRM ", sum(lm_data$death_cause == "NRM"), ", unknown ", sum(lm_data$death_cause == "unknown"),
  "); PFS events = ", sum(pfs_data$pfs_event), "\n",
  "Outputs: output/tables/", tag, "_*.csv, output/figures/", tag, "_*.png"
)
message("\n11_landmark_trajectory_6mo.R complete.")
