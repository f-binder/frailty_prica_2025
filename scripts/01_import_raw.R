# ==============================================================================
# 01_import_raw.R
#
# Purpose : Locate and import the raw Excel extracts - one "prior_*" and one
#           "updated_*" file, each holding a Baseline sheet and a Repeated
#           Measures sheet, plus (updated file only) a survival/outcomes
#           sheet. No recoding or derived variables here - just get the raw
#           values into R with reliable column names and types.
# Inputs  : data/raw/prior_*.xlsx
#           data/raw/updated_*.xlsx
# Outputs : data/processed/baseline_updated_raw.rds
#           data/processed/repeated_measures_updated_raw.rds
#           data/processed/survival_updated_raw.rds
#           data/processed/baseline_prior_raw.rds
#           data/processed/repeated_measures_prior_raw.rds
# Depends : 00_packages.R
#
# Note: only the *updated* extract is carried forward into 02-04. The prior
# extract is imported here too and kept in data/processed/ purely for
# provenance / version-reconciliation (e.g. checking what changed between
# data pulls) - see the project README for that comparison.
# ==============================================================================

source(here::here("scripts", "00_packages.R"))

## ---- Locate the raw files ------------------------------------------------
# Filenames carry the extraction date and change with every new data pull
# (e.g. "...-10Apr2026-clean.xlsx"), so we match on prefix rather than an
# exact name, and take the most recently modified file if more than one
# matches - each with a message() so it's obvious which file was actually
# used.

raw_dir <- here("data", "raw")
raw_dir

find_latest <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop("No file found in ", dir, " matching pattern: ", pattern)
  }
  if (length(files) > 1) {
    files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
    message(
      "Multiple files matched '", pattern, "' - using the most recently modified: ",
      basename(files[1])
    )
  }
  files[1]
}

updated_path <- find_latest(raw_dir, "^updated.*\\.xlsx$")
prior_path   <- find_latest(raw_dir, "^prior.*\\.xlsx$")

message("Updated extract: ", basename(updated_path))
message("Prior extract:   ", basename(prior_path))

## ---- Import: updated extract (primary, analysis-ready going forward) -----

baseline_updated_raw <- read_excel(
  updated_path,
  sheet = match_sheet(updated_path, "baseline")
) %>%
  rename_with(str_trim)

glimpse(baseline_updated_raw)

repeated_measures_updated_raw <- read_excel(
  updated_path,
  sheet = match_sheet(updated_path, "repeated measures")
) %>%
  rename_with(str_trim)

survival_updated_raw <- read_excel(
  updated_path,
  sheet = match_sheet(updated_path, "survival")
) %>%
  rename_with(str_trim)

## ---- Import: prior extract (kept for provenance / reconciliation only) ---

baseline_prior_raw <- read_excel(
  prior_path,
  sheet = match_sheet(prior_path, "baseline")
) %>%
  rename_with(str_trim)

repeated_measures_prior_raw <- read_excel(
  prior_path,
  sheet = match_sheet(prior_path, "repeated measures")
) %>%
  rename_with(str_trim)

## ---- Sanity checks ---------------------------------------------------
# Fail loudly here rather than silently downstream if a sheet came back empty
# or a study_id column is missing.

stopifnot(
  "baseline_updated_raw has 0 rows" = nrow(baseline_updated_raw) > 0,
  "repeated_measures_updated_raw has 0 rows" = nrow(repeated_measures_updated_raw) > 0,
  "survival_updated_raw has 0 rows" = nrow(survival_updated_raw) > 0,
  "study_id missing from baseline_updated_raw" = "study_id" %in% names(baseline_updated_raw),
  "study_id missing from repeated_measures_updated_raw" = "study_id" %in% names(repeated_measures_updated_raw),
  "study_id missing from survival_updated_raw" = "study_id" %in% names(survival_updated_raw)
)

message("\n--- Row / column counts ---")
message("baseline_updated_raw:          ", nrow(baseline_updated_raw), " rows x ", ncol(baseline_updated_raw), " cols")
message("repeated_measures_updated_raw: ", nrow(repeated_measures_updated_raw), " rows x ", ncol(repeated_measures_updated_raw), " cols")
message("survival_updated_raw:          ", nrow(survival_updated_raw), " rows x ", ncol(survival_updated_raw), " cols")
# message("baseline_prior_raw:            ", nrow(baseline_prior_raw), " rows x ", ncol(baseline_prior_raw), " cols")
# message("repeated_measures_prior_raw:   ", nrow(repeated_measures_prior_raw), " rows x ", ncol(repeated_measures_prior_raw), " cols")

glimpse(baseline_updated_raw)
glimpse(repeated_measures_updated_raw)
glimpse(survival_updated_raw)


## ---- Save ---------------------------------------------------------------

processed_dir <- here("data", "processed")
processed_dir
# dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(baseline_updated_raw, file.path(processed_dir, "baseline_updated_raw.rds"))
saveRDS(repeated_measures_updated_raw, file.path(processed_dir, "repeated_measures_updated_raw.rds"))
saveRDS(survival_updated_raw, file.path(processed_dir, "survival_updated_raw.rds"))
# saveRDS(baseline_prior_raw, file.path(processed_dir, "baseline_prior_raw.rds"))
# saveRDS(repeated_measures_prior_raw, file.path(processed_dir, "repeated_measures_prior_raw.rds"))

message("\n01_import_raw.R complete. Raw .rds files written to data/processed/.")
