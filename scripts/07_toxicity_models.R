# ==============================================================================
# 07_toxicity_models.R
#
# Purpose : Univariable (and, where the data support it, multivariable)
#           logistic regression for the three acute-toxicity outcomes: CRS,
#           ICANS, and ICU admission. Follows the same UVA covariate panel
#           and MVA-triggering logic as the study's own descriptive-
#           statistics report (fit an MVA only when at least two distinct
#           predictors reach significance in UVA), but applies it live to
#           all three outcomes on the matured data, and adds one safeguard
#           the original report didn't need: several ICU strata have very
#           few events, which produces (quasi-)complete separation for a
#           few covariates - flagged and excluded from MVA candidacy below
#           rather than silently reported as a normal estimate.
# Inputs  : data/processed/analysis_wide.rds
# Outputs : output/tables/uva_toxicity.csv
#           output/tables/mva_toxicity.csv  (only the outcomes for which an
#           MVA was actually fit)
# Depends : 00_packages.R, ..., 05_merge_analysis_datasets.R
# ==============================================================================

source(here::here("scripts", "00_packages.R"))

analysis_wide <- readRDS(here("data", "processed", "analysis_wide.rds"))
tables_dir <- here("output", "tables")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

## ---- Analysis-specific derived categories --------------------------------
# Clinically-established dichotomisations used alongside the continuous
# scores, matching the study's own descriptive-statistics report. Derived
# here (not in 02/05) since these are specific to this modelling choice,
# not a general property of the cleaned data.

d <- analysis_wide %>%
  mutate(
    ecog_cat = factor(
      case_when(ecog %in% c(0, 1) ~ "0-1", ecog %in% c(2, 3) ~ "2-3", TRUE ~ NA_character_),
      levels = c("0-1", "2-3")
    ),
    ves13_cat = factor(if_else(ves13_score < 3, "<3", ">=3"), levels = c("<3", ">=3")),
    cfs_cat = factor(if_else(cfs_score <= 3, "1-3", ">3"), levels = c("1-3", ">3")),
    walk_mpers_cat = factor(if_else(walk_mpers <= 0.80, "<=0.80", ">0.80"), levels = c("<=0.80", ">0.80"))
  )

## ---- UVA covariate panel --------------------------------------------------

uva_covariates <- c(
  "sex", "bridge_yn", "ecog", "ecog_cat", "age", "ldh", "crp", "albumin",
  "hctci_score", "cirs_total", "ves13_score", "ves13_cat",
  "cfs_score", "cfs_cat", "grip_average_kg", "walk_time", "walk_mpers",
  "walk_mpers_cat", "minicog_score", "phq_total_score"
)

toxicity_outcomes <- c("crs_yn", "icans_yn", "icu_yn")

## ---- UVA fitting helper ---------------------------------------------------

#' Run an expression while capturing every warning it raises (rather than
#' only the first stage of a multi-step fitting process), returning both
#' the expression's value and whether any captured warning indicates
#' (quasi-)complete separation. This matters here specifically because
#' glm() itself can converge without complaint while the *subsequent*
#' profile-likelihood CI computation inside broom::tidy(conf.int = TRUE)
#' throws its own separation warning - catching only the glm() call missed
#' exactly that case in testing.
run_flagging_separation <- function(expr) {
  fit_warnings <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      fit_warnings <<- c(fit_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, separation = any(str_detect(fit_warnings, "fitted probabilities numerically 0 or 1")))
}

#' Fit a single-predictor logistic regression and return a tidy summary row
#' per coefficient, including the predictor's overall (LR test) p-value and
#' a `separation` flag - see run_flagging_separation().
fit_uva_logistic <- function(data, outcome, covariate) {
  formula_str <- paste0(outcome, " ~ ", covariate)

  result <- run_flagging_separation({
    fit <- glm(as.formula(formula_str), data = data, family = binomial)
    list(
      fit = fit,
      overall_p = tryCatch(anova(fit, test = "Chisq")[2, "Pr(>Chi)"], error = function(e) NA_real_),
      tidy_fit = broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
    )
  })

  result$value$tidy_fit %>%
    filter(term != "(Intercept)") %>%
    mutate(
      covariate = covariate,
      outcome = outcome,
      n = nobs(result$value$fit),
      n_event = sum(result$value$fit$y),
      overall_p = result$value$overall_p,
      separation = result$separation
    ) %>%
    relocate(covariate, outcome)
}

uva_toxicity <- expand_grid(outcome = toxicity_outcomes, covariate = uva_covariates) %>%
  pmap_dfr(function(outcome, covariate) fit_uva_logistic(d, outcome, covariate))

n_separation <- sum(uva_toxicity$separation)
if (n_separation > 0) {
  message("--- (Quasi-)complete separation detected in ", n_separation, " coefficient(s) - excluded from MVA candidacy: ---")
  uva_toxicity %>%
    filter(separation) %>%
    distinct(outcome, covariate) %>%
    print(n = Inf)
}

message("\n--- UVA summary: covariates with overall p < 0.10, by outcome ---")
uva_toxicity %>%
  distinct(outcome, covariate, overall_p, separation) %>%
  filter(overall_p < 0.10) %>%
  arrange(outcome, overall_p) %>%
  print(n = Inf)

## ---- MVA candidate selection ----------------------------------------------
# Several UVA covariates are alternate parametrisations of the same
# clinical construct (a continuous score and its established
# dichotomisation). Treated as one construct here - if either form reached
# significance, the CONTINUOUS form is carried into the MVA (more
# statistically efficient at this sample size); constructs with no
# continuous form keep their own name. Rows flagged for separation are
# excluded before this selection runs at all.

construct_of <- c(
  ecog_cat = "ecog", ves13_cat = "ves13", cfs_cat = "cfs",
  walk_time = "walk", walk_mpers = "walk", walk_mpers_cat = "walk"
)
construct_of
preferred_form <- c(ecog = "ecog", ves13 = "ves13_score", cfs = "cfs_score", walk = "walk_mpers")

#' Given a UVA table (already restricted to one outcome), return the set of
#' covariate names to carry into an MVA: distinct clinical constructs with
#' overall p < p_threshold among non-separated fits, mapped to their
#' preferred (continuous, where one exists) representation.
select_mva_candidates <- function(uva_table, p_threshold = 0.05) {
  sig_raw <- uva_table %>%
    filter(!separation) %>%
    distinct(covariate, overall_p) %>%
    filter(overall_p < p_threshold) %>%
    pull(covariate)

  sig_constructs <- unique(unname(coalesce(construct_of[sig_raw], sig_raw)))
  unname(coalesce(preferred_form[sig_constructs], sig_constructs))
}

fit_mva_logistic <- function(data, outcome, covariates) {
  formula_str <- paste0(outcome, " ~ ", paste(covariates, collapse = " + "))

  result <- run_flagging_separation({
    fit <- glm(as.formula(formula_str), data = data, family = binomial)
    list(fit = fit, tidy_fit = broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE))
  })

  if (result$separation) {
    message(
      "  NOTE: the MVA model itself (", formula_str, ") shows signs of ",
      "(quasi-)complete separation when these predictors are combined, even ",
      "though neither did individually - its OR/CI should be treated as unstable."
    )
  }

  result$value$tidy_fit %>%
    filter(term != "(Intercept)") %>%
    mutate(
      outcome = outcome, n = nobs(result$value$fit), n_event = sum(result$value$fit$y),
      separation = result$separation
    ) %>%
    relocate(outcome)
}

mva_results <- list()

for (outc in toxicity_outcomes) {
  candidates <- select_mva_candidates(uva_toxicity %>% filter(outcome == outc))
  message("\n--- ", outc, ": MVA candidates = ", paste(candidates, collapse = ", "), " ---")

  if (length(candidates) >= 2) {
    mva_fit <- fit_mva_logistic(d, outc, candidates)
    print(mva_fit)
    mva_results[[outc]] <- mva_fit
  } else {
    message(
      "MVA not fit for ", outc, ": fewer than 2 non-separated, independent constructs ",
      "reached p < 0.05 in UVA (matches the original report's rule for when an MVA is warranted)."
    )
  }
}

mva_toxicity <- bind_rows(mva_results)
mva_toxicity

## ---- Save ---------------------------------------------------------------

write_csv(uva_toxicity, file.path(tables_dir, "uva_toxicity.csv"))
if (nrow(mva_toxicity) > 0) {
  write_csv(mva_toxicity, file.path(tables_dir, "mva_toxicity.csv"))
}

message("\n07_toxicity_models.R complete. Wrote uva_toxicity.csv",
        if (nrow(mva_toxicity) > 0) " and mva_toxicity.csv" else " (no MVA table - see messages above)")
