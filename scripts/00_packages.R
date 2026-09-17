# ==============================================================================
# 00_packages.R
#
# Purpose : Single place to install (if needed) and load every package used
#           anywhere in this project, set global options, and define small
#           project-wide helper functions. Every other script sources this
#           file first, so it can be run standalone (e.g. `Rscript
#           scripts/04_derive_survival_outcomes.R`) without depending on
#           what has already been run in the session.
# Inputs  : none
# Outputs : none (side effect: packages attached, options set, functions
#           defined in .GlobalEnv)
# ==============================================================================

## ---- Packages ---------------------------------------------------------

# Kept as one vector so a missing package is obvious and easy to add to.
required_pkgs <- c(
  "tidyverse",  # dplyr, tidyr, ggplot2, readr, purrr, stringr, forcats, lubridate
  "readxl",     # read the raw .xlsx extracts
  "here",       # project-relative paths that don't depend on the working directory
  "survival",   # Surv(), coxph(), survfit()
  "survminer",  # ggsurvplot() - ggplot2-based KM curves
  "cmprsk",     # cuminc(), crr() - competing-risks / Fine-Gray
  "flexmix",    # finite mixture models, used for CFS trajectory classification
  "broom",      # tidy() model objects into data frames
  "clipr"       # clipboard I/O, used by cw() below
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
missing_pkgs

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

## ---- Global options -----------------------------------------------------

theme_set(theme_minimal(base_size = 12))  # default ggplot2 theme for the project

## ---- Project-wide helper functions --------------------------------------

#' Copy a data frame / tibble to the clipboard (e.g. to paste into Excel/Sheets)
#'
#' @param x A data frame, tibble, or anything accepted by clipr::write_clip().
#' @return x, invisibly (via clipr's return_new = TRUE) - safe to leave at the
#'   end of a dplyr chain without breaking the pipeline.
cw <- function(x) {
  clipr::write_clip(x, return_new = TRUE)
}

#' Add proportion, cumulative proportion, and cumulative n to a count() output
#'
#' @param a_tibble A tibble with exactly one column literally named "n"
#'   (typically the output of dplyr::count()).
#' @return The same tibble with three added columns: p, cump, cumn.
add_pcump <- function(a_tibble) {
  if (sum(names(a_tibble) == "n") != 1) {
    stop("tibble needs to have exactly one 'n' column")
  }
  a_tibble %>%
    mutate(p = n / sum(n)) %>%
    mutate(
      cump = cumsum(p),
      cumn = cumsum(n)
    )
}

#' Find the one sheet in an .xlsx file whose (trimmed) name matches a pattern
#'
#' Excel sheet names in these data pulls have been inconsistent about
#' trailing whitespace (e.g. "Baseline " vs "Baseline"), and that has already
#' changed once between data pulls for this project. Matching on a trimmed,
#' case-insensitive pattern instead of a hard-coded literal name means the
#' import step keeps working if it happens again.
#'
#' @param path Path to the .xlsx file.
#' @param pattern Regex (case-insensitive) matched against trimmed sheet names.
#' @return The exact (untrimmed) sheet name, suitable for readxl::read_excel().
match_sheet <- function(path, pattern) {
  sheets <- readxl::excel_sheets(path)
  hit <- sheets[str_detect(str_to_lower(str_trim(sheets)), str_to_lower(pattern))]
  if (length(hit) != 1) {
    stop(sprintf(
      "Expected exactly one sheet matching '%s' in %s, found %d (sheets present: %s)",
      pattern, basename(path), length(hit), paste(sheets, collapse = "; ")
    ))
  }
  hit
}

message("00_packages.R: environment ready.")
