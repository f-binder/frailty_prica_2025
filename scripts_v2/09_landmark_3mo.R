# ==============================================================================
# 09_landmark_3mo.R
#
# Purpose : Post-infusion frailty, 3-month landmark analysis.
#   Cohort: patients alive (and under follow-up) at 3 months after infusion.
#   Exposure (only information available by 3 months):
#     A. Group-based trajectory model of CFS at baseline, 1 and 3 months
#        (lcmm::hlme with no random effects = group-based trajectory model,
#        the R equivalent of SAS PROC TRAJ with a normal outcome).
#        k = 1-4 groups, linear or quadratic time, 30 random starts each.
#        Selection: lowest BIC among models meeting adequacy criteria
#        (Nagin): every group >= 5% of patients, average posterior
#        probability >= 0.7, odds of correct classification >= 5.
#     B. Latest CFS by 3 months (continuous).
#   Outcomes measured FROM the landmark: OS; PFS (patients progression-free
#   at 3 months); RM and NRM (Fine-Gray and cause-specific Cox).
#   Adjusted models: + age + log2 LDH (and baseline CFS, early progression).
# Inputs  : data/processed/v2/analysis_wide.rds, analysis_long.rds
# Outputs : output/v2/tables/lm3_*.csv, output/v2/models/lm3_trajectory_model.rds,
#           data/processed/v2/lm3_data.rds, lm3_traj_long.rds (for figures)
#
# Caveats : with 3 time points a quadratic curve passes through every
#           patient's values, so "shape" information is limited; some nominal
#           3-month visits happened up to ~4 months after infusion (small
#           look-ahead past the landmark).
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "09_landmark_3mo.R"
suppressPackageStartupMessages(library(lcmm))   # masks cmprsk::cuminc -> always use cmprsk::
section(paste0("09 - ", LANDMARK_MONTHS, "-month landmark: post-infusion frailty"))

d  <- read_proc("analysis_wide")
al <- read_proc("analysis_long")
LM <- LANDMARK_MONTHS

## ---- Landmark cohort ----------------------------------------------------------------

flow <- d %>% summarise(
  infused = n(),
  died_before_lm = sum(os_months < LM & os_event == 1),
  censored_before_lm = sum(os_months < LM & os_event == 0),
  alive_at_lm = sum(os_months >= LM),
  progressed_before_lm = sum(os_months >= LM & pfs_event == 1 & pfs_months < LM))
print(flow)
save_table(flow, "lm3_cohort_flow")

cfs_window <- al %>%
  filter(visit_month <= LM, !is.na(cfs_score)) %>%
  group_by(study_id) %>%
  summarise(cfs_last = cfs_score[which.max(visit_month)], cfs_last_month = max(visit_month),
            cfs_m1 = cfs_score[visit_month == 1][1], cfs_m3 = cfs_score[visit_month == 3][1],
            n_cfs = n(), .groups = "drop")

lm <- d %>%
  filter(os_months >= LM) %>%
  left_join(cfs_window, by = "study_id") %>%
  mutate(os_lm = os_months - LM, pfs_lm = pfs_months - LM,
         prog_by_lm = as.integer(pfs_event == 1 & pfs_months < LM))
stopifnot("patient in landmark cohort without any CFS" = !anyNA(lm$cfs_last))

record("lm_n", nrow(lm))
record("lm_deaths", sum(lm$os_event))
message("Latest CFS by 3 months taken from month: ",
        paste(names(table(lm$cfs_last_month)), table(lm$cfs_last_month), sep = " = n ", collapse = "; "))

## ---- A. Group-based trajectory model ---------------------------------------------------

traj_data <- al %>%
  filter(study_id %in% lm$study_id, visit_month <= LM, !is.na(cfs_score)) %>%
  transmute(id = study_id, t = visit_month, cfs = cfs_score) %>%
  as.data.frame()

# Fitted class means at times tt. Class-membership intercepts share names with
# the trajectory intercepts in $best, so they (the first k-1 parameters) are dropped.
class_means <- function(m, tt) {
  k <- m$ng
  b <- if (k > 1) m$best[-seq_len(k - 1)] else m$best
  sapply(seq_len(k), function(j) {
    g <- function(n) { v <- b[if (k > 1) paste0(n, " class", j) else n]; ifelse(is.na(v), 0, v) }
    g("intercept") + g("t") * tt + g("I(t^2)") * tt^2
  })
}

fit_gbtm <- function(dat) {
  set.seed(SEED)
  fits <- list()
  for (form in c("linear", "quadratic")) {
    fx <- if (form == "linear") cfs ~ t else cfs ~ t + I(t^2)
    mx <- if (form == "linear") ~ t else ~ t + I(t^2)
    m1 <- hlme(fx, random = ~ -1, subject = "id", ng = 1, data = dat, verbose = FALSE)
    fits[[paste(form, 1)]] <- m1
    for (k in 2:4) {
      mk <- tryCatch(
        gridsearch(rep = 30, maxiter = 15, minit = m1,
                   hlme(fx, mixture = mx, random = ~ -1, subject = "id", ng = k, data = dat, verbose = FALSE)),
        error = function(e) NULL)
      if (!is.null(mk)) fits[[paste(form, k)]] <- mk
    }
  }
  diag <- imap_dfr(fits, function(m, nm) {
    k <- m$ng; pp <- m$pprob
    post <- as.matrix(pp[, grep("^prob", names(pp)), drop = FALSE])
    size <- as.numeric(table(factor(pp$class, levels = 1:k))) / nrow(pp)
    appa <- sapply(1:k, function(j) if (any(pp$class == j)) mean(post[pp$class == j, j]) else NA_real_)
    occ  <- (appa / (1 - appa)) / (size / (1 - size))
    tibble(model = nm, form = word(nm, 1), k = k, converged = m$conv == 1, loglik = m$loglik, BIC = m$BIC,
           min_group_pct = pct(min(size)), min_avepp = if (k > 1) min(appa) else 1,
           min_occ = if (k > 1) min(occ) else NA_real_,
           entropy = if (k > 1) 1 - sum(-post * log(pmax(post, 1e-12))) / (nrow(post) * log(k)) else NA_real_)
  }) %>%
    mutate(adequate = converged & k > 1 & min_group_pct >= 5 & min_avepp >= 0.7 & coalesce(min_occ, 0) >= 5)
  best <- diag %>% filter(adequate) %>% slice_min(BIC, n = 1)
  list(fits = fits, diag = diag, best_name = best$model, best = fits[[best$model]])
}

gbtm <- fit_gbtm(traj_data)
message("\nTrajectory model comparison (adequate = all groups >=5%, AvePP >=0.7, OCC >=5):")
print(gbtm$diag %>% mutate(across(c(loglik, BIC, min_avepp, min_occ, entropy), ~ round(.x, 2))), n = Inf)
save_table(gbtm$diag, "lm3_trajectory_model_selection")
message("Selected: ", gbtm$best_name)

best <- gbtm$best
k <- best$ng
means_0_1_3 <- class_means(best, c(0, 1, LM))
ord <- order(means_0_1_3[3, ])                    # order groups by fitted CFS at the landmark
group_labels <- if (k == 2) c("Lower frailty", "Higher frailty") else paste("Group", seq_len(k))

patient_group <- best$pprob %>%
  transmute(study_id = id, class,
            traj = factor(group_labels[match(class, ord)], levels = group_labels),
            post_prob = map2_dbl(seq_len(n()), class, ~ best$pprob[.x, paste0("prob", .y)]))
lm <- lm %>% left_join(patient_group %>% select(study_id, traj, post_prob), by = "study_id")

group_curves <- tibble(t = seq(0, LM, by = 0.1)) %>%
  bind_cols(as_tibble(class_means(best, seq(0, LM, by = 0.1)), .name_repair = ~ paste0("c", seq_along(.x)))) %>%
  pivot_longer(-t, names_to = "class", values_to = "cfs") %>%
  mutate(class = as.integer(str_remove(class, "c")),
         traj = factor(group_labels[match(class, ord)], levels = group_labels))
saveRDS(best, file.path(paths$models, "lm3_trajectory_model.rds"))

group_profile <- lm %>%
  group_by(traj) %>%
  summarise(n = n(), pct = pct(n() / nrow(lm), 1), mean_posterior = mean(post_prob),
            cfs_baseline = mean(cfs_score), cfs_latest = mean(cfs_last), baseline_cfs_gt3 = sum(cfs_score > 3),
            age_median = median(age), ldh_median = median(ldh), progressed_before_lm = sum(prog_by_lm),
            deaths = sum(os_event), rm = sum(cr_cause == 1), nrm = sum(cr_cause == 2), .groups = "drop") %>%
  mutate(nrm_pct = pct(nrm / n, 1))
fitted_tbl <- as_tibble(t(means_0_1_3[, ord, drop = FALSE]), .name_repair = ~ c("fit_m0", "fit_m1", "fit_m3")) %>%
  mutate(traj = factor(group_labels, levels = group_labels))
group_profile <- group_profile %>% left_join(fitted_tbl, by = "traj")
message("\nTrajectory groups:"); print(group_profile, width = Inf)
save_table(group_profile, "lm3_trajectory_groups")

lo_g <- group_profile %>% slice(1); hi_g <- group_profile %>% slice(n())
record("lm_traj_lower_pct", lo_g$pct, note = paste0("fitted CFS ", paste(round(c(lo_g$fit_m0, lo_g$fit_m1, lo_g$fit_m3), 1), collapse = " -> ")))
record("lm_traj_higher_pct", hi_g$pct, note = paste0("fitted CFS ", paste(round(c(hi_g$fit_m0, hi_g$fit_m1, hi_g$fit_m3), 1), collapse = " -> ")))
record("lm_traj_nrm_pct_lower", lo_g$nrm_pct, note = paste(lo_g$nrm, "of", lo_g$n))
record("lm_traj_nrm_pct_higher", hi_g$nrm_pct, note = paste(hi_g$nrm, "of", hi_g$n))

## ---- Outcomes by trajectory group ------------------------------------------------------

pfs_lm <- lm %>% filter(prog_by_lm == 0)
km_os  <- survfit(Surv(os_lm, os_event) ~ traj, data = lm)
km_pfs <- survfit(Surv(pfs_lm, pfs_event) ~ traj, data = pfs_lm)
km_tab <- bind_rows(km_at(km_os, c(12, 24)) %>% mutate(outcome = "OS"),
                    km_at(km_pfs, c(12, 24)) %>% mutate(outcome = "PFS")) %>%
  mutate(logrank_p = if_else(outcome == "OS", survdiff(Surv(os_lm, os_event) ~ traj, lm)$pvalue,
                             survdiff(Surv(pfs_lm, pfs_event) ~ traj, pfs_lm)$pvalue))
print(km_tab)
save_table(km_tab, "lm3_km_by_group")
os24 <- km_tab %>% filter(outcome == "OS", time == 24)
record("lm_os24_lower_pct", pct(os24$surv[1], 1), lo = pct(os24$lo[1], 1), hi = pct(os24$hi[1], 1), note = "24 mo after landmark")
record("lm_os24_higher_pct", pct(os24$surv[2], 1), lo = pct(os24$lo[2], 1), hi = pct(os24$hi[2], 1), note = "24 mo after landmark")

lm_models <- bind_rows(
  cox_fit("Surv(os_lm, os_event) ~ traj", lm) %>% mutate(outcome = "OS"),
  cox_fit("Surv(os_lm, os_event) ~ traj + age + log2_ldh", lm) %>% mutate(outcome = "OS"),
  cox_fit("Surv(os_lm, os_event) ~ traj + cfs_score", lm) %>% mutate(outcome = "OS"),
  cox_fit("Surv(pfs_lm, pfs_event) ~ traj", pfs_lm) %>% mutate(outcome = "PFS"),
  cox_fit("Surv(pfs_lm, pfs_event) ~ traj + age + log2_ldh", pfs_lm) %>% mutate(outcome = "PFS"),
  cox_fit("Surv(os_lm, os_event) ~ cfs_last", lm) %>% mutate(outcome = "OS"),
  cox_fit("Surv(os_lm, os_event) ~ cfs_last + age + log2_ldh", lm) %>% mutate(outcome = "OS"),
  cox_fit("Surv(os_lm, os_event) ~ cfs_last + age + log2_ldh + cfs_score", lm) %>% mutate(outcome = "OS"),
  cox_fit("Surv(os_lm, os_event) ~ cfs_last + age + log2_ldh + prog_by_lm", lm) %>% mutate(outcome = "OS"),
  cox_fit("Surv(os_lm, os_event) ~ cfs_score + age + log2_ldh", lm) %>% mutate(outcome = "OS"),
  cox_fit("Surv(pfs_lm, pfs_event) ~ cfs_last", pfs_lm) %>% mutate(outcome = "PFS"),
  cox_fit("Surv(pfs_lm, pfs_event) ~ cfs_last + age + log2_ldh", pfs_lm) %>% mutate(outcome = "PFS")
) %>% relocate(outcome)
save_table(lm_models %>% mutate(across(c(est, lo, hi, c_index), ~ round(.x, 3))), "lm3_cox_models")
message("\nLandmark Cox models (first term = exposure):")
print(lm_models %>% group_by(model) %>% slice(1) %>% ungroup() %>%
        transmute(outcome, model, hr = fmt_ci(est, lo, hi), p = fmt_p(p), n, events, c = round(c_index, 2)), n = Inf, width = Inf)

get_lm <- function(model, term) lm_models %>% filter(.data$model == !!model, .data$term == !!term)
r <- get_lm("Surv(os_lm, os_event) ~ traj", "trajHigher frailty");           record("lm_os_hr_traj", r$est, r$lo, r$hi, r$p)
r <- get_lm("Surv(os_lm, os_event) ~ traj + age + log2_ldh", "trajHigher frailty"); record("lm_os_hr_traj_adj", r$est, r$lo, r$hi, r$p)
r <- get_lm("Surv(pfs_lm, pfs_event) ~ traj", "trajHigher frailty");         record("lm_pfs_hr_traj", r$est, r$lo, r$hi, r$p)
r <- get_lm("Surv(os_lm, os_event) ~ cfs_last + age + log2_ldh", "cfs_last"); record("lm_os_hr_cfs3_adj", r$est, r$lo, r$hi, r$p)
r <- get_lm("Surv(os_lm, os_event) ~ cfs_last + age + log2_ldh + cfs_score", "cfs_last"); record("lm_os_hr_cfs3_adj_blcfs", r$est, r$lo, r$hi, r$p)
r <- get_lm("Surv(pfs_lm, pfs_event) ~ cfs_last + age + log2_ldh", "cfs_last"); record("lm_pfs_hr_cfs3_adj", r$est, r$lo, r$hi, r$p)

## ---- Competing risks from the landmark ---------------------------------------------------

lm_cr <- bind_rows(
  fg_fit(lm, "os_lm", "cr_cause", "traj", 1, "traj") ,
  fg_fit(lm, "os_lm", "cr_cause", "traj", 2, "traj"),
  fg_fit(lm, "os_lm", "cr_cause", c("cfs_last", ADJ), 1, "cfs_last + age + log2_ldh"),
  fg_fit(lm, "os_lm", "cr_cause", c("cfs_last", ADJ), 2, "cfs_last + age + log2_ldh"),
  fg_fit(lm, "os_lm", "cr_prog", c("cfs_last", ADJ), 1, "cfs_last + age + log2_ldh [standard def.]"),
  fg_fit(lm, "os_lm", "cr_prog", c("cfs_last", ADJ), 2, "cfs_last + age + log2_ldh [standard def.]")
) %>%
  filter(term %in% c("trajHigher frailty", "cfs_last")) %>%
  mutate(cause = case_when(str_detect(model, "standard") & failcode == 1 ~ "Death after progression",
                           str_detect(model, "standard") ~ "Death without progression",
                           failcode == 1 ~ "RM", TRUE ~ "NRM"), .before = 1)
message("\nLandmark Fine-Gray models:")
print(lm_cr %>% transmute(cause, model, shr = fmt_ci(est, lo, hi), p = fmt_p(p), events, converged))
save_table(lm_cr, "lm3_competing_risks")

r <- lm_cr %>% filter(cause == "NRM", model == "traj");              record("lm_nrm_shr_traj", r$est, r$lo, r$hi, r$p)
r <- lm_cr %>% filter(cause == "RM", model == "cfs_last + age + log2_ldh");  record("lm_rm_shr_cfs3_adj", r$est, r$lo, r$hi, r$p)
r <- lm_cr %>% filter(cause == "NRM", model == "cfs_last + age + log2_ldh"); record("lm_nrm_shr_cfs3_adj", r$est, r$lo, r$hi, r$p)

## ---- Sensitivity: trajectories from patients with >= 2 CFS assessments ---------------------

traj_data2 <- traj_data %>% group_by(id) %>% filter(n() >= 2) %>% ungroup() %>% as.data.frame()
gbtm2 <- fit_gbtm(traj_data2)
message("\nSensitivity (", n_distinct(traj_data2$id), " patients with >= 2 CFS values): selected ",
        gbtm2$best_name, " (", gbtm2$best$ng, " groups)")
save_table(gbtm2$diag, "lm3_trajectory_model_selection_sens_2obs")

## ---- Save data for figures --------------------------------------------------------------

save_proc(lm, "lm3_data")
save_proc(list(traj_long = al %>% filter(study_id %in% lm$study_id, visit_month <= LM, !is.na(cfs_score)) %>%
                 select(study_id, visit_month, cfs_score) %>% left_join(lm %>% select(study_id, traj), by = "study_id"),
               curves = group_curves, km = km_os), "lm3_figure_data")

message("09_landmark_3mo.R complete.")
