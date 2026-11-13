# ==============================================================================
# 10_figures.R
#
# Purpose : Publication figures (ggplot2), each saved as PNG (300 dpi), PDF and
#           a JPEG for abstract submission (<500 KB, black-and-white safe:
#           groups differ by line type / shape as well as colour).
#   Figure 1  Feasibility of each instrument (A) and baseline correlation
#             between frailty and fitness measures (B); alternative version
#             (fig1_..._core_instruments) whose panel A shows only instruments
#             in the protocol throughout (no EORTC QLQ-C30 / EQ-5D)
#   Figure 2  Frailty over the first year: CFS (A), gait speed (B), grip (C)
#   Figure 3  3-month landmark: CFS trajectory groups (A) and overall survival
#             from the landmark by group (B)
#   Figure 4  CFS over the first year: all available patients (A, subject to
#             survivor selection) and only patients alive at 12 months (B)
# Inputs  : output/v2/tables/feasibility_completion.csv, corr_baseline_pairs.csv
#           data/processed/v2/{analysis_long, trajectory_lsmeans, trajectory_lsmeans_12m_survivors,
#                               lm3_figure_data, lm3_data}.rds
# Outputs : output/v2/figures/fig1_feasibility_correlation.*, fig2_trajectories.*,
#           fig3_landmark_trajectory_survival.*, fig4_cfs_trajectory_all_vs_12m_survivors.*
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "10_figures.R"
section("10 - Publication figures")

al       <- read_proc("analysis_long")
lsmeans  <- read_proc("trajectory_lsmeans")
lsmeans_surv <- read_proc("trajectory_lsmeans_12m_survivors")
fig_lm   <- read_proc("lm3_figure_data")
lm       <- read_proc("lm3_data")
completion <- read_csv(file.path(paths$tables, "feasibility_completion.csv"), show_col_types = FALSE)
corr_pairs <- read_csv(file.path(paths$tables, "corr_baseline_pairs.csv"), show_col_types = FALSE)

GROUP_COLS  <- c(COL$blue, COL$orange)
GROUP_LTY   <- c("solid", "22")
GROUP_SHAPE <- c(16, 17)

## ---- Figure 1: feasibility + correlation ----------------------------------------------

feas <- completion %>%
  filter(visit != "All visits") %>%
  group_by(instrument, type) %>%
  summarise(min = min(pct_done), max = max(pct_done), .groups = "drop") %>%
  left_join(completion %>% filter(visit == "All visits") %>% select(instrument, overall = pct_done, visits), by = "instrument") %>%
  mutate(instrument = fct_reorder(instrument, overall))

make_p1a <- function(feas_data, subtitle) {
  ggplot(feas_data, aes(y = instrument)) +
    geom_segment(aes(x = min, xend = max, yend = instrument), colour = COL$light, linewidth = 2.2, lineend = "round") +
    geom_point(aes(x = overall), colour = COL$blue, size = 2.8) +
    geom_text(aes(x = overall, label = sprintf("%.0f%%", overall)), nudge_y = 0.36, size = 3, colour = COL$ink) +
    scale_x_continuous(limits = c(60, 101), breaks = seq(60, 100, 10), labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.01))) +
    labs(tag = "A", title = "Completion of each assessment", subtitle = subtitle, x = NULL, y = NULL) +
    theme_pub(10) +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#ecebe8", linewidth = 0.3))
}

p1a <- make_p1a(feas, "% of attended visits; point: all visits, bar: range across visits")

# Alternative panel A: only instruments in the protocol throughout the study
# (EORTC QLQ-C30 and EQ-5D were added on 27 Apr 2022 and are left out).
feas_core <- feas %>% filter(!instrument %in% LATER_ADDED_INSTRUMENTS) %>% mutate(instrument = droplevels(instrument))
p1a_core <- make_p1a(feas_core, "Instruments in the protocol throughout; % of attended visits;\npoint: all visits, bar: range across visits")

heat_vars <- c(cfs_score = "CFS", ves13_score = "VES-13", ecog = "ECOG", walk_mpers = "Gait speed",
               grip_average_kg = "Grip strength", minicog_score = "Mini-Cog", phq_total_score = "PHQ-9",
               cirs_total = "CIRS", age = "Age")
heat <- bind_rows(corr_pairs, corr_pairs %>% rename(var1 = var2, var2 = var1)) %>%
  filter(var1 %in% names(heat_vars), var2 %in% names(heat_vars)) %>%
  mutate(x = factor(heat_vars[var1], levels = heat_vars), y = factor(heat_vars[var2], levels = rev(heat_vars))) %>%
  filter(as.integer(x) < (length(heat_vars) + 1 - as.integer(y)))   # lower triangle only

p1b <- ggplot(heat, aes(x, y, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", rho), colour = abs(rho) > 0.45), size = 2.8, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = COL$ink)) +
  scale_fill_gradient2(low = COL$dblue, mid = COL$mid, high = COL$red, midpoint = 0, limits = c(-1, 1),
                       breaks = c(-1, -0.5, 0, 0.5, 1), name = "Spearman\nrho") +
  scale_x_discrete(position = "bottom") +
  coord_equal() +
  labs(tag = "B", title = "Correlation between baseline measures",
       subtitle = "Higher = frailer for CFS, VES-13, ECOG, PHQ-9, CIRS", x = NULL, y = NULL) +
  theme_pub(10) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right",
        legend.title = element_text(size = 9), legend.key.height = unit(1.2, "lines"), legend.key.width = unit(0.6, "lines"))

fig1 <- p1a + p1b + plot_layout(widths = c(1, 1.15))
save_figure(fig1, "fig1_feasibility_correlation", width = 11, height = 4.8)

# Alternative Figure 1 with the protocol-throughout panel A (panel B unchanged:
# it already contains no later-added instruments).
fig1_core <- p1a_core + p1b + plot_layout(widths = c(1, 1.15))
save_figure(fig1_core, "fig1_feasibility_correlation_core_instruments", width = 11, height = 4.8)

## ---- Figure 2: trajectories over the first year ------------------------------------------

# Visits are drawn evenly spaced (position 1-5), as is usual for scheduled-visit
# repeated-measures plots; the n assessed is printed under each visit.
ls_plot <- lsmeans %>% mutate(pos = as.integer(visit))

traj_panel <- function(v, ylab, title, tag, show_patients = FALSE, ylim = NULL,
                       ls_data = ls_plot, long_data = al) {
  dat <- ls_data %>% filter(var == v)
  p <- ggplot(dat, aes(pos, lsmean))
  if (show_patients) {
    indiv <- long_data %>% filter(!is.na(.data[[v]])) %>% transmute(study_id, pos = as.integer(visit), y = .data[[v]])
    p <- p + geom_line(data = indiv, aes(pos, y, group = study_id), colour = COL$grey, alpha = 0.35, linewidth = 0.3)
  }
  x_labels <- paste0(VISIT_LABELS, "\nn=", dat$n[match(seq_along(VISIT_LABELS), dat$pos)])
  p +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, colour = COL$blue, linewidth = 0.6) +
    geom_line(colour = COL$blue, linewidth = 1) +
    geom_point(colour = COL$blue, size = 2.4) +
    scale_x_continuous(breaks = seq_along(VISIT_LABELS), labels = x_labels, expand = expansion(add = 0.3)) +
    (if (!is.null(ylim)) coord_cartesian(ylim = ylim)) +
    labs(tag = tag, title = title, x = "Visit after CAR T infusion", y = ylab) +
    theme_pub(10)
}

p_cfs <- traj_panel("cfs_score", "Clinical Frailty Scale", "CFS: transient worsening at 1 month", "A",
                    show_patients = TRUE) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(subtitle = sprintf("Grey: individual patients; blue: model-estimated mean (95%% CI); p %s",
                          ifelse(lsmeans$p_overall[lsmeans$var == "cfs_score"][1] < 0.001, "< 0.001",
                                 sprintf("= %.3f", lsmeans$p_overall[lsmeans$var == "cfs_score"][1]))))
p_gait <- traj_panel("walk_mpers", "Gait speed (m/s)", "Gait speed", "B")
p_grip <- traj_panel("grip_average_kg", "Grip strength (kg)", "Grip strength", "C")

p_grip <- p_grip + labs(caption = "Later visits include only patients alive and assessed (survivor selection).") +
  theme(plot.caption = element_text(colour = COL$ink2, size = 8.5))
fig2 <- p_cfs / (p_gait | p_grip) + plot_layout(heights = c(1.25, 1))
save_figure(fig2, "fig2_trajectories", width = 8, height = 7.5)

fmt_p_traj <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p))

## ---- Figure 3: landmark trajectory groups and survival -----------------------------------

traj_long <- fig_lm$traj_long
curves <- fig_lm$curves
grp_n <- lm %>% count(traj) %>% mutate(lab = sprintf("%s (n=%d)", traj, n))
lab_map <- setNames(grp_n$lab, grp_n$traj)

p3a <- ggplot() +
  geom_line(data = traj_long, aes(visit_month, cfs_score, group = study_id, colour = traj),
            alpha = 0.3, linewidth = 0.35, position = position_jitter(width = 0, height = 0.07, seed = 1)) +
  geom_line(data = curves, aes(t, cfs, colour = traj, linetype = traj), linewidth = 1.4) +
  scale_colour_manual(values = GROUP_COLS, labels = lab_map) +
  scale_linetype_manual(values = GROUP_LTY, labels = lab_map) +
  scale_x_continuous(breaks = c(0, 1, 3)) +
  scale_y_continuous(breaks = 1:7, limits = c(0.7, 7.3)) +
  labs(tag = "A", title = "CFS trajectory groups, 0-3 months",
       subtitle = "Patients alive at 3 months; thick lines: fitted group means",
       x = "Months after CAR T infusion", y = "Clinical Frailty Scale") +
  theme_pub(10) +
  guides(colour = "none", linetype = "none")

# Kaplan-Meier from the landmark, drawn by hand (step curve + censor marks + risk table)
km <- fig_lm$km
km_df <- tibble(time = km$time, surv = km$surv, n_censor = km$n.censor,
                traj = rep(str_remove(names(km$strata), "^traj="), km$strata)) %>%
  group_by(traj) %>%
  group_modify(~ bind_rows(tibble(time = 0, surv = 1, n_censor = 0), .x)) %>%
  ungroup() %>%
  mutate(traj = factor(traj, levels = levels(lm$traj)))

hr <- read_csv(KEY_RESULTS_FILE, show_col_types = FALSE) %>% filter(key == "lm_os_hr_traj")
lr_p <- survdiff(Surv(os_lm, os_event) ~ traj, data = lm)$pvalue
x_max <- 60
breaks_x <- seq(0, x_max, 12)

p3b <- ggplot(km_df, aes(time, surv, colour = traj, linetype = traj)) +
  geom_step(linewidth = 1) +
  geom_point(data = filter(km_df, n_censor > 0), aes(shape = traj), size = 1.8, show.legend = FALSE) +
  annotate("text", x = 1, y = 0.08, hjust = 0, size = 3.1, colour = COL$ink,
           label = sprintf("HR %s; log-rank p = %.3f", fmt_ci(hr$value, hr$lo, hr$hi), lr_p)) +
  scale_colour_manual(values = GROUP_COLS, labels = lab_map) +
  scale_linetype_manual(values = GROUP_LTY, labels = lab_map) +
  scale_shape_manual(values = c(3, 3)) +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  scale_x_continuous(breaks = breaks_x) +
  coord_cartesian(xlim = c(0, x_max)) +
  labs(tag = "B", title = "Overall survival from the 3-month landmark",
       subtitle = "By CFS trajectory group", x = "Months after landmark", y = "Overall survival") +
  theme_pub(10)

risk <- summary(km, times = breaks_x, extend = TRUE)
risk_df <- tibble(time = risk$time, n = risk$n.risk,
                  traj = factor(str_remove(as.character(risk$strata), "^traj="), levels = levels(lm$traj)))
p3risk <- ggplot(risk_df, aes(time, fct_rev(traj), label = n, colour = traj)) +
  geom_text(size = 3, show.legend = FALSE) +
  scale_colour_manual(values = GROUP_COLS) +
  scale_x_continuous(breaks = breaks_x) +
  coord_cartesian(xlim = c(0, x_max)) +
  labs(x = NULL, y = NULL, subtitle = "Number at risk") +
  theme_pub(9) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
        panel.grid.major.y = element_blank(), plot.subtitle = element_text(size = 8.5))

fig3 <- (p3a | (p3b / p3risk + plot_layout(heights = c(5, 1)))) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_figure(fig3, "fig3_landmark_trajectory_survival", width = 11, height = 5.2)

## ---- Figure 4: CFS, all patients vs patients alive at 12 months ----------------------------

# A: every available assessment (as Figure 2A). Patients who die stop contributing,
#    so later visits over-represent fitter survivors.
# B: the same model restricted to patients alive at 12 months (OS >= 12 months),
#    whether or not they progressed: the trajectory of a 12-month survivor.
n_surv <- lsmeans_surv$n_patients[1]
al_surv <- al %>% filter(os_months >= 12)

p4a <- traj_panel("cfs_score", "Clinical Frailty Scale", "All patients with an assessment", "A",
                  show_patients = TRUE) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(x = NULL,
       subtitle = sprintf(paste0("Patients stop contributing at death or drop-out, so later visits over-represent\n",
                                 "fitter survivors (selection bias); model-estimated mean (95%% CI), p %s"),
                          fmt_p_traj(lsmeans$p_overall[lsmeans$var == "cfs_score"][1])))

p4b <- traj_panel("cfs_score", "Clinical Frailty Scale", sprintf("Patients alive at 12 months (n=%d)", n_surv), "B",
                  show_patients = TRUE, ls_data = mutate(lsmeans_surv, pos = as.integer(visit)), long_data = al_surv) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(subtitle = sprintf(paste0("Regardless of progression; removes selection by death (missed visits remain);\n",
                                 "model-estimated mean (95%% CI), p %s"),
                          fmt_p_traj(lsmeans_surv$p_overall[1])),
       caption = "Grey: individual patients. Patients censored alive before 12 months are excluded from B.") +
  theme(plot.caption = element_text(colour = COL$ink2, size = 8.5))

fig4 <- p4a / p4b
save_figure(fig4, "fig4_cfs_trajectory_all_vs_12m_survivors", width = 7, height = 8.5)

message("10_figures.R complete.")
