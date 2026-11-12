# ==============================================================================
# 01_import.R
#
# Purpose : Import the three sheets of the v2 raw extract (10 Apr 2026 cut,
#           with the reviewed causes of death) with reliable names and types.
#           No recoding here, except one documented parsing fix (CRP).
# Inputs  : data/raw/*v2*nrm reviewed*.xlsx
# Outputs : data/processed/v2/{baseline_raw, rm_raw, survival_raw}.rds
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "01_import.R"
section("01 - Import raw data")

## ---- Locate the raw file -------------------------------------------------------

raw_files <- list.files(paths$raw, pattern = RAW_FILE_PATTERN, ignore.case = TRUE, full.names = TRUE)
if (length(raw_files) != 1) {
  stop("Expected exactly one raw file matching '", RAW_FILE_PATTERN, "' in data/raw/, found ",
       length(raw_files), ": ", paste(basename(raw_files), collapse = "; "))
}
raw_path <- raw_files
message("Raw file: ", basename(raw_path))

# Sheet names carry trailing spaces ("Baseline "), so match on trimmed names.
find_sheet <- function(path, pattern) {
  sheets <- excel_sheets(path)
  hit <- sheets[str_detect(str_to_lower(str_trim(sheets)), pattern)]
  if (length(hit) != 1) stop("Sheet matching '", pattern, "' not found uniquely in ", basename(path))
  hit
}

read_sheet <- function(pattern) {
  read_excel(raw_path, sheet = find_sheet(raw_path, pattern), guess_max = 1000) %>%
    rename_with(str_trim)
}

baseline_raw <- read_sheet("^baseline")
rm_raw       <- read_sheet("^repeated measures")
survival_raw <- read_sheet("survival")

message(sprintf("Baseline: %d x %d | Repeated measures: %d x %d | Survival: %d x %d",
                nrow(baseline_raw), ncol(baseline_raw), nrow(rm_raw), ncol(rm_raw),
                nrow(survival_raw), ncol(survival_raw)))

## ---- Structural checks -------------------------------------------------------------

stopifnot(
  "study_id missing from a sheet" =
    all(c("study_id") %in% names(baseline_raw), "study_id" %in% names(rm_raw), "study_id" %in% names(survival_raw)),
  "duplicate study_id in baseline" = !anyDuplicated(baseline_raw$study_id),
  "duplicate study_id in survival" = !anyDuplicated(survival_raw$study_id),
  "duplicate study_id x timepoint in repeated measures" = !anyDuplicated(rm_raw[, c("study_id", "timepoint")])
)

if (!"reviewed_death_disease" %in% names(survival_raw)) {
  warning("Column 'reviewed_death_disease' not found: cause of death will use 'death_disease' only.")
  survival_raw$reviewed_death_disease <- NA_real_
}

## ---- Types ----------------------------------------------------------------------------
# readxl types a whole column as text if one cell is text. CRP holds "<1.0"
# (below the detection limit) for some patients; it is set to 0.1 mg/L, as in
# the v1 pipeline and the statistician's report, so results stay comparable.
# (CRP is only used descriptively.)

numeric_cols <- c("age", "ldh", "crp", "albumin", "hctci_score", "cirs_total", "ves13_score",
                  "karnofsky", "ecog", "cart_line", "los_days")

message("CRP values that are not plain numbers: ",
        paste(unique(baseline_raw$crp[is.na(suppressWarnings(as.numeric(baseline_raw$crp)))]), collapse = ", "))

baseline_raw <- baseline_raw %>%
  mutate(crp = if_else(str_detect(as.character(crp), fixed("<")), 0.1,
                       suppressWarnings(as.numeric(crp))))

not_numeric <- numeric_cols[!vapply(baseline_raw[numeric_cols], is.numeric, logical(1))]
if (length(not_numeric) > 0) stop("Columns expected numeric but imported as text: ", paste(not_numeric, collapse = ", "))

## ---- Save -------------------------------------------------------------------------------

save_proc(baseline_raw, "baseline_raw")
save_proc(rm_raw, "rm_raw")
save_proc(survival_raw, "survival_raw")
save_proc(tibble(raw_file = basename(raw_path), imported = Sys.time()), "import_info")

message("01_import.R complete.")
