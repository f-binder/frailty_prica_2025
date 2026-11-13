# ==============================================================================
# 05_correlations.R
#
# Purpose : Explore the frailty measures and how they relate to each other.
#   A. Distributions at baseline: missingness, floor/ceiling effects, skew
#   B. Baseline correlation matrix (Spearman, pairwise complete), with 95% CI
#   C. Agreement between categorical cut-offs (CFS >3, VES-13 >=3, ECOG 2-3,
#      gait <=0.8 m/s)
#   D. Correlation of CFS with each measure at every visit (cross-sectional)
#   E. Within-patient (repeated-measures) correlation across visits: do two
#      measures move together over time within the same patient?
#   F. Correlation of CHANGES from baseline to 1 month
#   G. Tracking: how well does baseline CFS predict later CFS?
# Inputs  : data/processed/v2/analysis_wide.rds, analysis_long.rds
# Outputs : output/v2/tables/corr_*.csv, explore_*.csv,
#           output/v2/figures/explore_baseline_distributions.png, plus the
#           alternatives restricted to measures collected throughout the study:
#           explore_baseline_distributions_protocol_throughout.png,
#           explore_corr_baseline_protocol_throughout.png
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "05_correlations.R"
section("05 - Exploration and correlation between frailty measures")

d  <- read_proc("analysis_wide")
al <- read_proc("analysis_long")

# Measures included in the correlation analyses, with display labels.
# Direction note: higher = frailer for CFS, VES-13, ECOG, CIRS, HCT-CI, PHQ-9,
# walk time; higher = fitter for grip, gait speed, Mini-Cog, Karnofsky, EORTC.
measures <- c(
  cfs_score              = "CFS",
  ves13_score            = "VES-13",
  walk_mpers             = "Gait speed",
  grip_average_kg        = "Grip strength",
  minicog_score          = "Mini-Cog",
  phq_total_score        = "PHQ-9",
  ecog                   = "ECOG",
  karnofsky              = "Karnofsky",
  cirs_total             = "CIRS",
  hctci_score            = "HCT-CI",
  eortc_physicalfunction = "EORTC physical",
  age                    = "Age"
)

## ---- A. Distributions at baseline ------------------------------------------------------

bl_long <- d %>%
  select(study_id, all_of(names(measures))) %>%
  pivot_longer(-study_id, names_to = "var", values_to = "value") %>%
  mutate(measure = factor(measures[var], levels = measures))

# floor / ceiling: share of patients at the minimum / maximum possible score
scale_limits <- tribble(
  ~var,             ~floor, ~ceiling,
  "cfs_score",      1,      9,
  "ves13_score",    0,      10,
  "minicog_score",  0,      5,
  "phq_total_score", 0,     27,
  "ecog",           0,      4,
  "karnofsky",      10,     100,
  "eortc_physicalfunction", 0, 100
)

distributions <- bl_long %>%
  group_by(var, measure) %>%
  summarise(n = sum(!is.na(value)), missing = sum(is.na(value)),
            median = median(value, na.rm = TRUE), q1 = quantile(value, 0.25, na.rm = TRUE),
            q3 = quantile(value, 0.75, na.rm = TRUE), min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE), mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE), n_distinct = n_distinct(value[!is.na(value)]),
            .groups = "drop") %>%
  left_join(scale_limits, by = "var") %>%
  left_join(bl_long %>% left_join(scale_limits, by = "var") %>% group_by(var) %>%
              summarise(pct_floor = pct(mean(value == floor, na.rm = TRUE), 1),
                        pct_ceiling = pct(mean(value == ceiling, na.rm = TRUE), 1), .groups = "drop"),
            by = "var") %>%
  arrange(measure) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))
message("Baseline distributions (floor/ceiling = % at the scale minimum/maximum):")
print(distributions %>% select(measure, n, missing, median, q1, q3, min, max, n_distinct, pct_floor, pct_ceiling), n = Inf)
save_table(distributions, "explore_baseline_distributions")
message("Notes: Mini-Cog has a strong ceiling effect and PHQ-9 a floor effect, which limits",
        " the correlations they can show with other measures.")

p_dist <- ggplot(bl_long %>% filter(!is.na(value)), aes(value)) +
  geom_histogram(bins = 15, fill = COL$blue, colour = "white", linewidth = 0.3) +
  facet_wrap(~ measure, scales = "free", ncol = 4) +
  labs(x = NULL, y = "Patients", title = "Baseline distribution of frailty and fitness measures",
       subtitle = "Exploratory; n = 78 patients (fewer where a measure is missing)") +
  theme_pub(10)
ggsave(file.path(paths$figures, "explore_baseline_distributions.png"), p_dist, width = 9, height = 6.5,
       dpi = 200, device = ragg::agg_png, bg = "white")

# Alternative: only measures collected throughout the study period (excludes
# instruments added to the protocol on 27 Apr 2022, e.g. EORTC QLQ-C30, EQ-5D).
measures_always <- measures[!str_detect(names(measures), LATER_ADDED_VAR_REGEX)]
message("Measures collected throughout the study: ", paste(measures_always, collapse = ", "),
        " | excluded (added later): ", paste(setdiff(measures, measures_always), collapse = ", "))

p_dist_always <- ggplot(bl_long %>% filter(!is.na(value), var %in% names(measures_always)), aes(value)) +
  geom_histogram(bins = 15, fill = COL$blue, colour = "white", linewidth = 0.3) +
  facet_wrap(~ measure, scales = "free", ncol = 4) +
  labs(x = NULL, y = "Patients", title = "Baseline distribution of frailty and fitness measures",
       subtitle = "Measures collected throughout the study only (later-added questionnaires excluded)") +
  theme_pub(10)
ggsave(file.path(paths$figures, "explore_baseline_distributions_protocol_throughout.png"), p_dist_always,
       width = 9, height = 6.5, dpi = 200, device = ragg::agg_png, bg = "white")

## ---- Spearman helper with 95% CI ----------------------------------------------------------
# CI by Fisher z with the Bonett & Wright (2000) variance for Spearman's rho:
# var(z) = (1 + rho^2 / 2) / (n - 3).

spearman_ci <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  n <- sum(ok)
  if (n < 5) return(tibble(rho = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_, n = n))
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  rho <- unname(ct$estimate)
  se <- sqrt((1 + rho^2 / 2) / (n - 3))
  z <- atanh(rho)
  tibble(rho = rho, lo = tanh(z - 1.96 * se), hi = tanh(z + 1.96 * se), p = ct$p.value, n = n)
}

## ---- B. Baseline correlation matrix -------------------------------------------------------

pairs <- combn(names(measures), 2, simplify = FALSE)
corr_baseline <- map_dfr(pairs, function(pr) {
  spearman_ci(d[[pr[1]]], d[[pr[2]]]) %>%
    mutate(var1 = pr[1], var2 = pr[2], measure1 = measures[pr[1]], measure2 = measures[pr[2]], .before = 1)
})
save_table(corr_baseline %>% mutate(across(c(rho, lo, hi), ~ round(.x, 2))), "corr_baseline_pairs")

message("\nBaseline correlations with CFS (Spearman, 95% CI) ", SIG_LEGEND, ":")
cfs_corr <- corr_baseline %>% filter(var1 == "cfs_score") %>% arrange(desc(abs(rho)))
print(cfs_corr %>% transmute(measure2, rho = round(rho, 2), ci = fmt_ci(rho, lo, hi), p = fmt_p_star(p), n))

message("\nStrongest baseline correlations overall (|rho| >= 0.4):")
print(corr_baseline %>% filter(abs(rho) >= 0.4) %>% arrange(desc(abs(rho))) %>%
        transmute(measure1, measure2, rho = round(rho, 2), n), n = Inf)

rho_cfs <- function(v) cfs_corr %>% filter(var2 == v)
for (v in c("ves13_score", "walk_mpers", "ecog", "grip_average_kg", "minicog_score", "age", "phq_total_score")) {
  r <- rho_cfs(v)
  record(paste0("rho_baseline_cfs_", v), r$rho, lo = r$lo, hi = r$hi, p = r$p, note = paste("n =", r$n))
}
mc <- corr_baseline %>% filter((var1 == "minicog_score" | var2 == "minicog_score"),
                               (var1 %in% c("cfs_score", "walk_mpers", "grip_average_kg") |
                                  var2 %in% c("cfs_score", "walk_mpers", "grip_average_kg")))
record("rho_baseline_minicog_physical_max_abs", max(abs(mc$rho)), note = "Mini-Cog vs CFS, gait, grip")
ag <- corr_baseline %>% filter((var1 == "age" | var2 == "age"),
                               (var1 %in% c("cfs_score", "walk_mpers", "grip_average_kg") |
                                  var2 %in% c("cfs_score", "walk_mpers", "grip_average_kg")))
record("rho_baseline_age_physical_max_abs", max(abs(ag$rho)), note = "age vs CFS, gait, grip")

# Correlation heatmap, measures collected throughout the study only
# (exploratory: all of them, including Karnofsky and HCT-CI, unlike Figure 1B).
always_levels <- unname(measures_always)
heat_always <- bind_rows(corr_baseline, corr_baseline %>% rename(var1 = var2, var2 = var1, measure1 = measure2, measure2 = measure1)) %>%
  filter(var1 %in% names(measures_always), var2 %in% names(measures_always)) %>%
  mutate(x = factor(measure1, levels = always_levels), y = factor(measure2, levels = rev(always_levels))) %>%
  filter(as.integer(x) < (length(always_levels) + 1 - as.integer(y)))   # lower triangle
p_corr_always <- ggplot(heat_always, aes(x, y, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", rho), colour = abs(rho) > 0.45), size = 2.7, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = COL$ink)) +
  scale_fill_gradient2(low = COL$dblue, mid = COL$mid, high = COL$red, midpoint = 0, limits = c(-1, 1),
                       name = "Spearman\nrho") +
  coord_equal() +
  labs(title = "Baseline correlation between measures collected throughout the study",
       subtitle = "Pairwise complete; higher = frailer for CFS, VES-13, ECOG, PHQ-9, CIRS, HCT-CI", x = NULL, y = NULL) +
  theme_pub(10) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right",
        legend.title = element_text(size = 9))
ggsave(file.path(paths$figures, "explore_corr_baseline_protocol_throughout.png"), p_corr_always,
       width = 8.5, height = 7, dpi = 200, device = ragg::agg_png, bg = "white")

## ---- C. Agreement between categorical cut-offs ----------------------------------------------

cuts <- d %>% transmute(
  `CFS >3` = cfs_score > 3, `VES-13 >=3` = ves13_score >= 3, `ECOG 2-3` = ecog >= 2,
  `Gait <=0.8 m/s` = walk_mpers <= GAIT_SLOW_MPS, `Karnofsky <=70%` = karnofsky <= 70)
cut_pairs <- combn(names(cuts), 2, simplify = FALSE)
agreement <- map_dfr(cut_pairs, function(pr) {
  a <- cuts[[pr[1]]]; b <- cuts[[pr[2]]]; ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]
  po <- mean(a == b); pe <- mean(a) * mean(b) + mean(!a) * mean(!b)
  tibble(cut1 = pr[1], cut2 = pr[2], n = sum(ok), both = sum(a & b), only1 = sum(a & !b),
         only2 = sum(!a & b), neither = sum(!a & !b), agreement_pct = pct(po, 1),
         kappa = round((po - pe) / (1 - pe), 2))
})
message("\nAgreement between frailty cut-offs (Cohen's kappa):"); print(agreement, n = Inf)
save_table(agreement, "corr_cutoff_agreement")
message("\nCFS by ECOG (baseline):"); print(table(CFS = d$cfs_score, ECOG = d$ecog, useNA = "ifany"))

## ---- D. Correlation of CFS with other measures, by visit --------------------------------------

visit_measures <- c(walk_mpers = "Gait speed", grip_average_kg = "Grip strength",
                    minicog_score = "Mini-Cog", phq_total_score = "PHQ-9",
                    eortc_physicalfunction = "EORTC physical", eortc_globalhealth = "EORTC global health",
                    esas_tiredness = "ESAS tiredness")
corr_by_visit <- al %>%
  group_by(visit) %>%
  group_modify(function(x, ...) map_dfr(names(visit_measures), function(v)
    spearman_ci(x$cfs_score, x[[v]]) %>% mutate(measure = visit_measures[v], .before = 1))) %>%
  ungroup()
corr_by_visit_wide <- corr_by_visit %>%
  transmute(measure, visit, cell = sprintf("%.2f (n=%d)", rho, n)) %>%
  pivot_wider(names_from = visit, values_from = cell)
message("\nSpearman correlation of CFS with each measure, by visit:"); print(corr_by_visit_wide, width = Inf)
save_table(corr_by_visit %>% mutate(across(c(rho, lo, hi), ~ round(.x, 2))), "corr_cfs_by_visit")

post <- corr_by_visit %>% filter(visit %in% c("1 mo", "3 mo", "6 mo"))
g <- post %>% filter(measure == "Gait speed")
record("rho_post_cfs_gait_min", min(g$rho), note = "1-6 months; strongest negative")
record("rho_post_cfs_gait_max", max(g$rho), note = "1-6 months; weakest")
record("rho_12m_cfs_gait", corr_by_visit$rho[corr_by_visit$visit == "12 mo" & corr_by_visit$measure == "Gait speed"],
       note = "n ~20; much weaker")
ph <- post %>% filter(measure == "PHQ-9")
record("rho_post_cfs_phq_min", min(ph$rho), note = "1-6 months")
record("rho_post_cfs_phq_max", max(ph$rho), note = "1-6 months")

## ---- E. Within-patient (repeated-measures) correlation ------------------------------------------
# rmcorr (Bakdash & Marusich 2017): fit y ~ patient + x by least squares; the
# within-patient correlation is sign(beta) * sqrt(SS_x / (SS_x + SS_residual)).
# It asks whether, WITHIN a patient, visits with a higher CFS also have, e.g.,
# slower gait - independent of between-patient differences.

rmcorr <- function(data, x, y) {
  dd <- data %>% select(study_id, xv = all_of(x), yv = all_of(y)) %>% filter(!is.na(xv), !is.na(yv)) %>%
    group_by(study_id) %>% filter(n() >= 2) %>% ungroup() %>% mutate(study_id = factor(study_id))
  if (n_distinct(dd$study_id) < 5) return(tibble(r_rm = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_, patients = 0L, obs = 0L))
  fit <- lm(yv ~ study_id + xv, data = dd)
  a <- anova(fit)
  ss_x <- a["xv", "Sum Sq"]; ss_res <- a["Residuals", "Sum Sq"]; df_res <- a["Residuals", "Df"]
  r <- sign(coef(fit)["xv"]) * sqrt(ss_x / (ss_x + ss_res))
  se_z <- 1 / sqrt(df_res - 1)
  tibble(r_rm = unname(r), lo = tanh(atanh(r) - 1.96 * se_z), hi = tanh(atanh(r) + 1.96 * se_z),
         p = a["xv", "Pr(>F)"], patients = n_distinct(dd$study_id), obs = nrow(dd))
}
corr_within <- map_dfr(names(visit_measures), function(v)
  rmcorr(al, "cfs_score", v) %>% mutate(measure = visit_measures[v], .before = 1))
message("\nWithin-patient correlation of CFS with each measure across visits (rmcorr) ", SIG_LEGEND, ":")
print(corr_within %>% mutate(across(c(r_rm, lo, hi), ~ round(.x, 2)), p = fmt_p_star(p)))
save_table(corr_within, "corr_cfs_within_patient")

## ---- F. Correlation of changes, baseline -> 1 month ------------------------------------------

chg <- al %>%
  filter(visit_month %in% c(0, 1)) %>%
  select(study_id, visit_month, cfs_score, all_of(names(visit_measures))) %>%
  pivot_wider(names_from = visit_month, values_from = -c(study_id, visit_month), names_sep = "_m")
corr_change <- map_dfr(names(visit_measures), function(v) {
  spearman_ci(chg$cfs_score_m1 - chg$cfs_score_m0, chg[[paste0(v, "_m1")]] - chg[[paste0(v, "_m0")]]) %>%
    mutate(measure = visit_measures[v], .before = 1)
})
message("\nCorrelation of the CHANGE in CFS (baseline -> 1 mo) with the change in each measure ", SIG_LEGEND, ":")
print(corr_change %>% mutate(across(c(rho, lo, hi), ~ round(.x, 2)), p = fmt_p_star(p)))
save_table(corr_change, "corr_change_0_1m")

## ---- G. Tracking of CFS over time ----------------------------------------------------------------

cfs_wide <- al %>% select(study_id, visit, cfs_score) %>% pivot_wider(names_from = visit, values_from = cfs_score)
tracking <- map_dfr(VISIT_LABELS[-1], function(v)
  spearman_ci(cfs_wide$Baseline, cfs_wide[[v]]) %>% mutate(visit = v, .before = 1))
message("\nBaseline CFS vs CFS at later visits (Spearman):"); print(tracking %>% mutate(across(c(rho, lo, hi), ~ round(.x, 2))))
save_table(tracking, "corr_cfs_tracking")

message("05_correlations.R complete.")
