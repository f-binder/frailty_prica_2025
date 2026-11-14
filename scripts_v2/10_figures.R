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
#   Figure 6  Baseline distribution of the main frailty instruments, each with
#             a bar of the proportion completed at baseline (feasibility)
#             (Figure 5, the alluvial plot, is made by 12_alluvial_cfs.R)
# Structure: every figure is built as an object first (fig1, fig2, ...); all
#           files are written in the final "Save" section. To work
#           interactively, set SAVE_FIGURES <- FALSE, run the script, then
#           print / adjust the objects (e.g. fig2 + plot_layout(heights = ...))
#           and save with save_figure(<object>, "<name>", width, height).
#           Trajectory panels use traj_panel() from 00_setup.R.
# Inputs  : output/v2/tables/feasibility_completion.csv, corr_baseline_pairs.csv
#           data/processed/v2/{analysis_long, trajectory_lsmeans, trajectory_lsmeans_12m_survivors,
#                               lm3_figure_data, lm3_data}.rds
# Outputs : output/v2/figures/fig1_feasibility_correlation.*, fig2_trajectories.*,
#           fig3_landmark_trajectory_survival.*, fig4_cfs_trajectory_all_vs_12m_survivors.*,
#           fig6_baseline_distribution_completion.*
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "10_figures.R"
if (!exists("SAVE_FIGURES")) SAVE_FIGURES <- TRUE   # set FALSE before sourcing to only build the plots
section("10 - Publication figures")

al       <- read_proc("analysis_long")
lsmeans  <- read_proc("trajectory_lsmeans")
lsmeans_surv <- read_proc("trajectory_lsmeans_12m_survivors")
wide     <- read_proc("analysis_wide")
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
               setNames("Grip strength", GRIP_VAR), minicog_score = "Mini-Cog", phq_total_score = "PHQ-9",
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

# Alternative Figure 1 with the protocol-throughout panel A (panel B unchanged:
# it already contains no later-added instruments).
fig1_core <- p1a_core + p1b + plot_layout(widths = c(1, 1.15))

## ---- Figure 2: trajectories over the first year ------------------------------------------

# Visits are placed on a true time scale (months 0, 1, 3, 6, 12 after infusion);
# the n assessed is printed under each visit (traj_panel() is in 00_setup.R).
ls_plot <- lsmeans %>% mutate(pos = visit_pos(visit))

p_cfs <- traj_panel("cfs_score", "Clinical Frailty Scale", "CFS: transient worsening at 1 month", "A",
                    ls_data = ls_plot, long_data = al, show_patients = TRUE) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(subtitle = sprintf("Grey: individual patients; blue: model-estimated mean (95%% CI); p %s",
                          ifelse(lsmeans$p_overall[lsmeans$var == "cfs_score"][1] < 0.001, "< 0.001",
                                 sprintf("= %.3f", lsmeans$p_overall[lsmeans$var == "cfs_score"][1]))))
p_gait <- traj_panel("walk_mpers", "Gait speed (m/s)", "Gait speed", "B", ls_data = ls_plot)
p_grip <- traj_panel(GRIP_VAR, GRIP_LABEL, "Grip strength", "C", ls_data = ls_plot)

p_grip <- p_grip + labs(caption = "Later visits include only patients alive and assessed (survivor selection).") +
  theme(plot.caption = element_text(colour = COL$ink2, size = 8.5))
fig2 <- p_cfs / (p_gait | p_grip) + plot_layout(heights = c(1.25, 1))

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

## ---- Figure 4: CFS, all patients vs patients alive at 12 months ----------------------------

# A: every available assessment (as Figure 2A). Patients who die stop contributing,
#    so later visits over-represent fitter survivors.
# B: the same model restricted to patients alive at 12 months (OS >= 12 months),
#    whether or not they progressed: the trajectory of a 12-month survivor.
n_surv <- lsmeans_surv$n_patients[1]
al_surv <- al %>% filter(os_months >= 12)

p4a <- traj_panel("cfs_score", "Clinical Frailty Scale", "All patients with an assessment", "A",
                  ls_data = ls_plot, long_data = al, show_patients = TRUE) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(x = NULL,
       subtitle = sprintf(paste0("Patients stop contributing at death or drop-out, so later visits over-represent\n",
                                 "fitter survivors (selection bias); model-estimated mean (95%% CI), p %s"),
                          fmt_p_traj(lsmeans$p_overall[lsmeans$var == "cfs_score"][1])))

p4b <- traj_panel("cfs_score", "Clinical Frailty Scale", sprintf("Patients alive at 12 months (n=%d)", n_surv), "B",
                  show_patients = TRUE, ls_data = mutate(lsmeans_surv, pos = visit_pos(visit)), long_data = al_surv) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(subtitle = sprintf(paste0("Regardless of progression; removes selection by death (missed visits remain);\n",
                                 "model-estimated mean (95%% CI), p %s"),
                          fmt_p_traj(lsmeans_surv$p_overall[1])),
       caption = "Grey: individual patients. Patients censored alive before 12 months are excluded from B.") +
  theme(plot.caption = element_text(colour = COL$ink2, size = 8.5))

fig4 <- p4a / p4b

## ---- Figure 6: baseline distribution + completion at baseline -------------------------------

# Completion = instrument scored at the baseline visit, out of all 78 patients
# (everyone attended baseline). Bar colour runs from mustard (<80% completed)
# to dark orange-red (>95%) so the less feasible instruments stand out.
base_inst <- tribble(
  ~var,              ~label,               ~binwidth, ~discrete, ~breaks,
  "cfs_score",       "CFS",                1,     TRUE,  1:7,
  "ves13_score",     "VES-13",             1,     TRUE,  0:7,
  "walk_mpers",      "Gait speed (m/s)",   0.1,   FALSE, seq(0.5, 1.75, 0.25),
  "grip",            "Grip strength",      NA,    FALSE, NULL,   # filled from GRIP_VAR below
  "minicog_score",   "Mini-Cog",           1,     TRUE,  1:5,
  "phq_total_score", "PHQ-9",              1,     TRUE,  seq(0, 15, 3)
)
grip_hist <- list(
  grip_z          = list(label = "Grip strength (z vs age/sex norms)", binwidth = 0.5, breaks = seq(-5, 3, 1)),
  grip_pctpred    = list(label = "Grip strength (% predicted)",        binwidth = 10,  breaks = seq(0, 160, 40)),
  grip_average_kg = list(label = "Grip strength (kg)",                 binwidth = 5,   breaks = seq(0, 60, 10))
)[[GRIP_VAR]]
i_grip <- which(base_inst$var == "grip")
base_inst$var[i_grip] <- GRIP_VAR
base_inst$label[i_grip] <- grip_hist$label
base_inst$binwidth[i_grip] <- grip_hist$binwidth
base_inst$breaks[[i_grip]] <- grip_hist$breaks
base_comp <- base_inst %>%
  mutate(n = nrow(wide), done = map_int(var, ~ sum(!is.na(wide[[.x]]))), pct = 100 * done / n,
         txt = sprintf("Completed %d/%d (%.0f%%)", done, n, pct))
print(base_comp %>% select(label, done, n, pct))

COMP_LIMITS <- c(75, 100)
comp_fill <- scale_fill_gradientn(
  colours = c("#b8860b", "#b8860b", "#d9622b", "#b5302a", "#b5302a"),
  values  = scales::rescale(c(75, 80, 90, 95, 100), from = COMP_LIMITS),
  limits = COMP_LIMITS, oob = scales::squish, guide = "none")

base_panel <- function(i) {
  r  <- base_inst[i, ]
  cc <- base_comp[i, ]
  d  <- tibble(value = wide[[r$var]]) %>% filter(!is.na(value))
  h <- ggplot(d, aes(value)) +
    geom_histogram(binwidth = r$binwidth, boundary = if (r$discrete) r$binwidth / 2 else 0,
                   fill = COL$blue, colour = "white", linewidth = 0.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = r$breaks[[1]]) +
    labs(title = r$label, x = NULL, y = "Patients") +
    theme_pub(9.5) +
    theme(panel.grid.major.x = element_blank(), plot.margin = margin(4, 6, 0, 6))
  b <- ggplot(cc, aes(y = 1)) +
    geom_col(aes(x = 100), fill = COL$light, width = 0.75, orientation = "y") +
    geom_col(aes(x = pct, fill = pct), width = 0.75, orientation = "y") +
    geom_text(aes(x = 2.5, label = txt), hjust = 0, size = 2.9, colour = "white", fontface = "bold") +
    comp_fill +
    scale_x_continuous(limits = c(0, 100.1), expand = c(0, 0)) +
    theme_void() +
    theme(plot.margin = margin(3, 6, 6, 6))
  h / b + plot_layout(heights = c(7, 1))
}

fig6 <- wrap_plots(lapply(seq_len(nrow(base_inst)), base_panel), ncol = 3) +
  plot_annotation(
    title = "Baseline frailty assessment: distribution of results and completion",
    subtitle = paste0("Histograms: patients with a result. Bar: proportion of the 78 patients with the test completed at baseline\n",
                      "(colour from mustard, <80% completed, to dark orange-red, >95%)"),
    theme = theme(plot.title = element_text(face = "bold", size = 12),
                  plot.subtitle = element_text(colour = COL$ink2, size = 9.5)))

## ---- Save ---------------------------------------------------------------------------------

if (SAVE_FIGURES) {
  save_figure(fig1,      "fig1_feasibility_correlation",                  width = 11, height = 4.8)
  save_figure(fig1_core, "fig1_feasibility_correlation_core_instruments", width = 11, height = 4.8)
  save_figure(fig2,      "fig2_trajectories",                             width = 8,  height = 7.5)
  save_figure(fig3,      "fig3_landmark_trajectory_survival",             width = 11, height = 5.2)
  save_figure(fig4,      "fig4_cfs_trajectory_all_vs_12m_survivors",      width = 7,  height = 8.5)
  save_figure(fig6,      "fig6_baseline_distribution_completion",         width = 10, height = 6.8)
}

message("10_figures.R complete.")
