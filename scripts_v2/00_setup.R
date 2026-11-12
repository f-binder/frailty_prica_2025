# ==============================================================================
# 00_setup.R
#
# Purpose : Shared setup for the v2 analysis pipeline (scripts_v2/). Loads
#           packages, defines paths, analysis constants, plotting theme, and
#           small helper functions used by every other script. Each script
#           sources this file first, so any script can be run on its own
#           (after its upstream scripts have been run once).
#
# Pipeline (run in order, or use scripts_v2/run_all.R):
#   01_import.R               raw Excel (v2, NRM reviewed) -> raw .rds
#   02_clean_derive.R         cleaning, survival outcomes, cause of death,
#                             data-quality flags, analysis datasets
#   03_descriptives.R         Table 1, events, follow-up, OS/PFS, CIF
#   04_feasibility.R          completion of each instrument by visit
#   05_correlations.R         exploration + correlation between measures
#   06_trajectories.R         mixed models of frailty measures over time
#   07_toxicity_los.R         toxicity, ICU and length of stay
#   08_survival_baseline.R    OS/PFS, RM vs NRM, time-varying Cox
#   09_landmark_3mo.R         3-month landmark: trajectory groups, CFS at 3 mo
#   10_figures.R              publication figures (3)
#   11_check_abstract.R       compares computed numbers with the abstract
#
# Outputs : data/processed/v2/*.rds, output/v2/{tables,figures,models}/
#           Nothing here writes to data/raw/ or to the v1 folders.
# ==============================================================================

## ---- Packages ---------------------------------------------------------------
# MASS and cmprsk are NOT attached: MASS::select() masks dplyr::select(), and
# lcmm (attached in 09) masks cmprsk::cuminc(). Both are always called with ::.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(survival)
  library(patchwork)
  library(scales)
})

required_pkgs <- c("tidyverse", "readxl", "here", "survival", "cmprsk", "lcmm",
                   "MASS", "nlme", "patchwork", "scales", "broom", "ragg")
missing_pkgs <- required_pkgs[!nzchar(vapply(required_pkgs, function(p) system.file(package = p), character(1)))]
if (length(missing_pkgs) > 0) {
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
       ". Install them with install.packages() before running the pipeline.")
}

options(dplyr.summarise.inform = FALSE, pillar.width = 120, width = 120)

## ---- Paths --------------------------------------------------------------------

paths <- list(
  raw     = here("data", "raw"),
  proc    = here("data", "processed", "v2"),
  tables  = here("output", "v2", "tables"),
  figures = here("output", "v2", "figures"),
  models  = here("output", "v2", "models")
)
invisible(lapply(paths[-1], dir.create, recursive = TRUE, showWarnings = FALSE))

# Raw file used by this pipeline: the version with the reviewed causes of death.
RAW_FILE_PATTERN <- "v2.*nrm reviewed.*\\.xlsx$"

## ---- Analysis constants ------------------------------------------------------

DATA_CUT        <- as.Date("2026-04-10")  # date of the data extract
DAYS_PER_MONTH  <- 30.4375                # average month length
LANDMARK_MONTHS <- 3                      # landmark for post-infusion frailty
LOS_PROLONGED   <- 14                     # "prolonged" index admission, days
GAIT_SLOW_MPS   <- 0.80                   # slow gait speed threshold, m/s
# EORTC QLQ-C30, EQ-5D and FACT-Cog were added in protocol version of
# 27 April 2022; patients enrolled earlier were not asked to complete them.
QOL_PROTOCOL_START <- as.Date("2022-04-27")
SEED            <- 20261111

VISITS <- c("baseline" = 0, "1 month" = 1, "3 months" = 3, "6 months" = 6, "12 months" = 12)
VISIT_LABELS <- c("Baseline", "1 mo", "3 mo", "6 mo", "12 mo")

# Pre-specified adjustment set for "adjusted" models: age and disease burden.
ADJ <- c("age", "log2_ldh")

## ---- Plot theme and palette -------------------------------------------------
# Two-group colours: slots 1-2 of the validated reference categorical palette
# (blue, orange). Every two-group figure ALSO uses line type / shape, so the
# figures remain readable in black and white (meeting requirement).

COL <- list(
  blue   = "#2a78d6",
  orange = "#eb6834",
  ink    = "#0b0b0b",
  ink2   = "#52514e",
  grey   = "#a9a8a3",
  light  = "#d9d8d4",
  mid    = "#f0efec",   # diverging midpoint
  red    = "#c23b3a",   # diverging warm pole
  dblue  = "#1c5cab"    # diverging cool pole
)

theme_pub <- function(base_size = 11) {
  theme_classic(base_size = base_size, base_family = "") +
    theme(
      text             = element_text(colour = COL$ink),
      axis.text        = element_text(colour = COL$ink2),
      axis.line        = element_line(colour = COL$ink2, linewidth = 0.4),
      axis.ticks       = element_line(colour = COL$ink2, linewidth = 0.4),
      panel.grid.major.y = element_line(colour = "#ecebe8", linewidth = 0.3),
      plot.title       = element_text(face = "bold", size = rel(1.05)),
      plot.subtitle    = element_text(colour = COL$ink2),
      plot.tag         = element_text(face = "bold"),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.key.width = unit(1.6, "lines"),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

#' Save a figure as PNG (300 dpi), PDF, and a JPEG for abstract submission.
#' The JPEG is checked against the meeting's 500 KB limit.
save_figure <- function(plot, name, width = 7, height = 5) {
  # patchwork + ggplot2 4.x emit a harmless "annotation$theme is not a valid
  # theme" warning on every save; muffle only that one.
  ggsave <- function(...) withCallingHandlers(ggplot2::ggsave(...), warning = function(w) {
    if (grepl("annotation\\$theme", conditionMessage(w))) invokeRestart("muffleWarning")
  })
  png_file <- file.path(paths$figures, paste0(name, ".png"))
  pdf_file <- file.path(paths$figures, paste0(name, ".pdf"))
  jpg_file <- file.path(paths$figures, paste0(name, ".jpg"))
  ggsave(png_file, plot, width = width, height = height, dpi = 300, device = ragg::agg_png, bg = "white")
  # base pdf device: cairo_pdf is not reliably available on macOS builds
  ggsave(pdf_file, plot, width = width, height = height, device = grDevices::pdf, bg = "white",
         useDingbats = FALSE)
  # JPEG for submission: ~1,650 px wide, sized to stay under 500 KB
  ggsave(jpg_file, plot, width = width, height = height, dpi = 150, device = ragg::agg_jpeg,
         quality = 85, bg = "white")
  kb <- file.size(jpg_file) / 1024
  message(sprintf("Saved %s (.png/.pdf/.jpg; jpg = %.0f KB%s)", name, kb,
                  if (kb > 500) " - OVER the 500 KB submission limit" else ""))
  invisible(png_file)
}

## ---- Small helpers --------------------------------------------------------------

save_table <- function(x, name) {
  write_csv(x, file.path(paths$tables, paste0(name, ".csv")), na = "")
  invisible(x)
}

read_proc <- function(name) readRDS(file.path(paths$proc, paste0(name, ".rds")))
save_proc <- function(x, name) saveRDS(x, file.path(paths$proc, paste0(name, ".rds")))

section <- function(txt) message("\n", strrep("=", 78), "\n", txt, "\n", strrep("=", 78))

pct <- function(x, digits = 0) round(100 * x, digits)

#' Format an estimate with 95% CI, e.g. "1.32 (1.08-1.63)".
fmt_ci <- function(est, lo, hi, digits = 2) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f-%.", digits, "f)"), est, lo, hi)
}

fmt_p <- function(p) ifelse(is.na(p), NA_character_,
                     ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

## ---- Key-results registry ---------------------------------------------------------
# Every number quoted in the abstract is written to one table
# (output/v2/tables/key_results.csv) by the script that computes it, so
# 11_check_abstract.R can compare all of them with the abstract text in one place.

KEY_RESULTS_FILE <- file.path(paths$tables, "key_results.csv")

record <- function(key, value, lo = NA_real_, hi = NA_real_, p = NA_real_, note = "") {
  script <- get0("SCRIPT_NAME", ifnotfound = NA_character_)
  new <- tibble(key = key, value = as.numeric(value), lo = as.numeric(lo),
                hi = as.numeric(hi), p = as.numeric(p), note = note,
                script = script, updated = format(Sys.time(), "%Y-%m-%d %H:%M"))
  old <- if (file.exists(KEY_RESULTS_FILE)) {
    read_csv(KEY_RESULTS_FILE, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
      mutate(across(c(value, lo, hi, p), as.numeric))
  } else NULL
  out <- bind_rows(filter(old %||% new[0, ], key != !!key), new) %>% arrange(key)
  write_csv(out, KEY_RESULTS_FILE, na = "")
  invisible(new)
}

## ---- Model helpers ----------------------------------------------------------------

#' Tidy one Cox model term: HR, 95% CI, p, N, events, concordance.
cox_term <- function(fit, term = NULL) {
  s <- summary(fit)
  ci <- s$conf.int
  term <- term %||% rownames(ci)[1]
  tibble(term = term, est = ci[term, 1], lo = ci[term, 3], hi = ci[term, 4],
         p = s$coefficients[term, "Pr(>|z|)"], n = fit$n, events = fit$nevent,
         c_index = unname(fit$concordance["concordance"]))
}

#' Fit a Cox model from a formula string and return all terms tidied.
cox_fit <- function(formula_str, data) {
  fit <- coxph(as.formula(formula_str), data = data)
  s <- summary(fit)
  tibble(model = formula_str, term = rownames(s$conf.int),
         est = s$conf.int[, 1], lo = s$conf.int[, 3], hi = s$conf.int[, 4],
         p = s$coefficients[, "Pr(>|z|)"], n = fit$n, events = fit$nevent,
         c_index = unname(fit$concordance["concordance"]))
}

#' Fine-Gray subdistribution hazard model (cmprsk::crr) for one cause.
#' `vars` are column names; factors are expanded with model.matrix.
#' Rows with missing covariates are dropped.
fg_fit <- function(data, time, status, vars, failcode, label = NULL) {
  mf <- data[, c(time, status, vars)]
  mf <- mf[complete.cases(mf), ]
  X <- model.matrix(as.formula(paste("~", paste(vars, collapse = " + "))), mf)[, -1, drop = FALSE]
  fit <- cmprsk::crr(ftime = mf[[time]], fstatus = mf[[status]], cov1 = X,
                     failcode = failcode, cencode = 0)
  s <- summary(fit)
  # computed outside tibble(): inside it, `failcode` would refer to the new column
  n_events <- sum(mf[[status]] == failcode)
  tibble(model = label %||% paste(vars, collapse = " + "), failcode = failcode,
         term = rownames(s$coef), est = s$conf.int[, 1], lo = s$conf.int[, 3],
         hi = s$conf.int[, 4], p = s$coef[, "p-value"], n = nrow(mf),
         events = n_events, converged = fit$converged)
}

#' Cumulative incidence (cmprsk::cuminc) at given times, overall or by group.
cif_at <- function(time, status, times, group = NULL) {
  ci <- if (is.null(group)) cmprsk::cuminc(time, status) else cmprsk::cuminc(time, status, group)
  est <- cmprsk::timepoints(ci, times)
  as_tibble(est$est, rownames = "curve") %>%
    pivot_longer(-curve, names_to = "time", values_to = "cif") %>%
    mutate(time = as.numeric(time),
           group = str_remove(curve, " \\d+$"),
           cause = as.integer(str_extract(curve, "\\d+$"))) %>%
    select(group, cause, time, cif)
}

#' Kaplan-Meier estimates at given times (overall or by strata).
km_at <- function(fit, times) {
  s <- summary(fit, times = times, extend = TRUE)
  tibble(strata = if (is.null(s$strata)) "All" else str_remove(as.character(s$strata), "^.*="),
         time = s$time, n_risk = s$n.risk, surv = s$surv, lo = s$lower, hi = s$upper)
}

#' Area under the ROC curve (Mann-Whitney), for a binary outcome and a score.
auc_mw <- function(score, outcome) {
  ok <- !is.na(score) & !is.na(outcome)
  score <- score[ok]; outcome <- outcome[ok]
  r <- rank(score)
  n1 <- sum(outcome == 1); n0 <- sum(outcome == 0)
  (sum(r[outcome == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

message("00_setup.R: environment ready (outputs -> output/v2/, data -> data/processed/v2/).")
