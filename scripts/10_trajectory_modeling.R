# ==============================================================================
# 10_trajectory_modeling.R
#
# Purpose : Group-based trajectory modelling (Nagin-style) of the Clinical
#           Frailty Scale (CFS) over the first year post-CAR-T: fit
#           candidate models varying both the number of latent classes (k)
#           and the within-class functional form of time (linear vs.
#           quadratic), select the best-supported model by BIC, and assign
#           each patient to their most likely class.
#
#           Deliberately out of scope here (left for a later script): the
#           survival analysis linking class membership to OS/PFS. This
#           script's job is the trajectory model itself.
# Inputs  : data/processed/repeated_measures_clean.rds
# Outputs : output/tables/trajectory_model_comparison.csv
#           output/tables/patient_trajectory_class.csv
#           output/figures/trajectory_classes.png
#           output/models/trajectory_fit.rds  (the winning flexmix object)
# Depends : 00_packages.R, ..., 03_clean_repeated_measures.R
#
# Method note
# -------------------------------------------------
# flexmix (finite mixture of regressions) is used rather than a dedicated
# GBTM package (lcmm, traj), which were not available in this environment.
# flexmix's `group` argument is what makes this a genuine group-based
# trajectory model rather than an unstructured mixture: it constrains every
# repeated observation from the same patient to be assigned to the same
# latent class, exactly as Nagin's original GBTM formulation requires.
# ==============================================================================

source(here::here("scripts", "00_packages.R"))
library(flexmix)

set.seed(20260910)  # fixed for a reproducible class-label assignment

tables_dir <- here("output", "tables")
figures_dir <- here("output", "figures")
models_dir <- here("output", "models")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

repeated_measures_clean <- readRDS(here("data", "processed", "repeated_measures_clean.rds"))

traj_data <- repeated_measures_clean %>%
  filter(!is.na(cfs_score)) %>%
  mutate(study_id = factor(study_id))

message("Patients contributing to the trajectory model: ", n_distinct(traj_data$study_id))
message("Total CFS observations: ", nrow(traj_data))

## ---- Fit candidate models: k = 1..3, linear and quadratic time ----------
# nrep = 15 random-start refits per (k, form) combination, to reduce the
# chance that a single unlucky random initialisation drives the model
# selection (flexmix's EM fit can converge to a local optimum).

fit_k <- function(k, quadratic = FALSE, nrep = 15) {
  form <- if (quadratic) {
    cfs_score ~ visit_month + I(visit_month^2) | study_id
  } else {
    cfs_score ~ visit_month | study_id
  }
  stepFlexmix(form, data = traj_data, k = k, nrep = nrep, control = list(verbose = 0))
}

candidate_grid <- expand_grid(k = 1:3, quadratic = c(FALSE, TRUE))
candidate_fits <- map2(candidate_grid$k, candidate_grid$quadratic, fit_k)

model_comparison <- candidate_grid %>%
  mutate(
    form = if_else(quadratic, "quadratic", "linear"),
    AIC = map_dbl(candidate_fits, AIC),
    BIC = map_dbl(candidate_fits, BIC),
    k_fitted = map_int(candidate_fits, ~ .x@k)  # flexmix can converge to fewer non-empty
  ) %>%                                          # components than requested (k)
  select(k_requested = k, form, k_fitted, AIC, BIC)

message("\n--- Model comparison (k requested vs. fitted, AIC/BIC) ---")
print(model_comparison)

best_idx <- which.min(model_comparison$BIC)
best_fit <- candidate_fits[[best_idx]]

message(
  "\nBest model by BIC: k=", model_comparison$k_fitted[best_idx],
  ", ", model_comparison$form[best_idx], " (BIC=", round(model_comparison$BIC[best_idx], 1), ")"
)

if (model_comparison$k_fitted[best_idx] < 2) {
  stop(
    "The best-supported model has only 1 class - there is no meaningful ",
    "trajectory heterogeneity to report. Stopping rather than forcing a ",
    "multi-class solution onto the rest of the script."
  )
}

## ---- Class assignment and classification-quality diagnostics ------------

k_final <- model_comparison$k_fitted[best_idx]

message("\n--- Class-specific coefficients (winning model) ---")
print(parameters(best_fit))

posterior_probs <- posterior(best_fit)
traj_data$class <- clusters(best_fit)
traj_data$map_prob <- apply(posterior_probs, 1, max)  # this observation's posterior prob. of its assigned class

patient_class <- traj_data %>%
  distinct(study_id, class) %>%
  left_join(
    traj_data %>% group_by(study_id) %>% summarise(mean_map_prob = mean(map_prob), n_obs = n(), .groups = "drop"),
    by = "study_id"
  ) %>%
  mutate(study_id = as.integer(as.character(study_id)))

message("\n--- Class sizes (patients) ---")
print(table(patient_class$class))

message("Mean posterior assignment probability (higher = more confident classification): ",
        round(mean(patient_class$mean_map_prob), 3))

# Normalised entropy (Ramaswamy et al. 1993): 0 = no better than chance
# assignment, 1 = perfect separation. >0.7 is conventionally "good".
entropy_raw <- -sum(posterior_probs * log(posterior_probs + 1e-12))
entropy_norm <- 1 - entropy_raw / (nrow(posterior_probs) * log(ncol(posterior_probs)))
message("Normalised entropy: ", round(entropy_norm, 3))

## ---- Trajectory plot: observed data by class + class-specific fit -------

patient_class_counts <- patient_class %>% count(class, name = "n_patients")

class_labels <- traj_data %>%
  group_by(class) %>%
  summarise(mean_baseline_cfs = mean(cfs_score[visit_month == 0], na.rm = TRUE), .groups = "drop") %>%
  left_join(patient_class_counts, by = "class") %>%
  arrange(mean_baseline_cfs) %>%
  mutate(class_label = paste0("Class ", row_number(), " (n=", n_patients, " patients)"))

traj_plot_data <- traj_data %>%
  left_join(class_labels %>% select(class, class_label), by = "class")

# Each observation's fitted CFS value under its OWN assigned class -
# computed from fitted(best_fit) directly (a matrix, one column per
# component) rather than by re-deriving coefficients by name, which would
# be fragile across the linear/quadratic model specifications.
fitted_by_class <- tibble(
  visit_month = traj_data$visit_month,
  class = traj_data$class,
  fitted_cfs = fitted(best_fit)[cbind(seq_len(nrow(traj_data)), traj_data$class)]
) %>%
  distinct() %>%
  left_join(class_labels %>% select(class, class_label), by = "class") %>%
  arrange(class, visit_month)

traj_class_plot <- ggplot() +
  geom_line(
    data = traj_plot_data,
    aes(x = visit_month, y = cfs_score, group = study_id, color = class_label),
    alpha = 0.25, linewidth = 0.3
  ) +
  geom_line(
    data = fitted_by_class,
    aes(x = visit_month, y = fitted_cfs, color = class_label),
    linewidth = 1.2
  ) +
  scale_x_continuous(breaks = c(0, 1, 3, 6, 12)) +
  scale_color_manual(values = c("#2C7BB6", "#D7191C")) +
  labs(
    title = "CFS trajectory classes (group-based trajectory model)",
    subtitle = paste0(
      "k=", k_final, " (", model_comparison$form[best_idx], "), entropy=", round(entropy_norm, 2)
    ),
    x = "Months from CAR-T infusion", y = "CFS score", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(figures_dir, "trajectory_classes.png"), plot = traj_class_plot, width = 7.5, height = 6, dpi = 300)

## ---- Save ---------------------------------------------------------------

write_csv(model_comparison, file.path(tables_dir, "trajectory_model_comparison.csv"))
write_csv(patient_class, file.path(tables_dir, "patient_trajectory_class.csv"))
saveRDS(best_fit, file.path(models_dir, "trajectory_fit.rds"))

message("\n10_trajectory_modeling.R complete. Wrote 2 CSVs, 1 figure, and the winning model object.")
