# ==============================================================================
# 02_clean_baseline.R
#
# Purpose : Recode the raw baseline sheet into an analysis-ready tibble:
#           labelled factors for categorical fields, a corrected numeric
#           scale for Karnofsky, explicit Date type for the infusion date,
#           and a data-quality flag column carried forward for anything
#           that could not be recoded with confidence.
# Inputs  : data/processed/baseline_updated_raw.rds
# Outputs : data/processed/baseline_clean.rds
# Depends : 00_packages.R, 01_import_raw.R
#
# A note on how the codings below were determined
# -------------------------------------------------
# Codings are taken directly from the study's official REDCap code book
# (`Frailty - Code Book - 09Jul2025.docx`; PI Dr. Anca Prica, REB# 20-5617),
#
#
# The code book documents several fields not present in this particular
# extract at all (race_*, education_years, first_language, Gender,
# tx_lines). 
# See data/data_dictionary.md for the fuller write-up, including items the code
# book does *not* resolve (e.g. eos_reason, Death_disease's "Unknown"
# level) - those live in the survival sheet, out of scope for this script.
# ==============================================================================

# source(here::here("scripts", "00_packages.R"))

baseline_raw <- readRDS(here("data", "processed", "baseline_updated_raw.rds"))
glimpse(baseline_raw)


## ---- Small helpers for the repeated yes/no [/ unknown] patterns ----------

recode_yn <- function(x) {
  factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

recode_yn_unknown <- function(x) {
  factor(x, levels = c(0, 1, 2), labels = c("No", "Yes", "Unknown"))
}

## --- check IDs in new vs old
  # duplicates?
map(list(baseline_prior_raw, baseline_raw),
    function(x) x %>% janitor::get_dupes(study_id))

setdiff(baseline_prior_raw$study_id, baseline_raw$study_id) # a previous ID 76 is now missing

  # check missing case (in prior dataset)
baseline_prior_raw %>% filter(study_id == 76) %>%
  relocate(study_id, cart_product) %>% glimpse()


## ---- check coded variables as reported in the codebook ---------------

# sex - code book: 0 Male, 1 Female, 2 Other, 3 Not reported (only 0/1 occur
# in this cohort; the other two levels are kept for correctness if a
# future data pull includes them)

baseline_raw %>%
  count(sex) %>% add_pcump()

# diagnosis - code book: 0 Lymphoma, 1 Myeloma, 2 Leukemia (only 0 occurs - this is
# a lymphoma-specific study cohort)
baseline_raw %>%
  count(diagnosis) %>% add_pcump()

# lym_diagnosis - code book: 0 Non-Hodgkin's Lymphoma, 1 Other (specify in lym_other)
baseline_raw %>%
  count(lym_diagnosis) %>% add_pcump()

# lym_nhl - code book: 0 DLBCL, 1 High-grade B-cell lymphoma, 2 Primary
# mediastinal B-cell lymphoma, 3 Transformed DLBCL from follicular
# lymphoma, 4 Follicular lymphoma, 5 Mantle cell lymphoma
baseline_raw %>%
  count(lym_nhl) %>% add_pcump()

baseline_raw %>%
  count(lym_diagnosis, lym_nhl) %>% add_pcump() # here, other + lym_nhl would be considered missing (n=3)


# rel_trans_yn - code book: 1 Yes, 0 No, 2 Unknown - a genuine 3-level field (unlike
# most of the other *_yn fields below), only 0/1 observed here
baseline_raw %>%
  count(rel_trans_yn) %>% add_pcump()


# cns_current - code book: 1 Yes, 0 No, 2 Unknown
   # these values changed - presumably reviewed
baseline_raw %>%
  count(cns_current) %>% add_pcump()

baseline_prior_raw %>% count(cns_current) %>% add_pcump()
  

# cart_product - code book: 0 Kymriah, 1 Yescarta, 2 JCAR Trial, 3 Tecartus
baseline_raw %>%
  count(cart_product) %>% add_pcump()

# yes/no for the following:
map(list("crs_yn", "icans_yn", "icu_yn", "bridge_yn", "asct", "asct_relapse", 
  "active_mal", "allosct_yn", "ae_yn"),
  function(nombre) baseline_raw %>% count(pick(one_of(nombre))) %>% add_pcump())

  # some should be correlative:
  baseline_raw %>% count(asct, asct_relapse) # OK

  # seems like blood or metabolic AEs was coded yes vs missing (so NA = no) - no changes from prior dataset


# karnofsky - code book: Karnofsky is stored as a 1-10 INDEX (1 = 10%, 2 = 20%, ...,
# 10 = 100%), not as the percentage itself.
  
baseline_raw %>% count(karnofsky)

# ecog - no transformation needed
baseline_raw %>% count(ecog) %>% add_pcump()


## ---- Clean -----------------------------------------------------------

baseline_clean <- baseline_raw %>%
  mutate(
    study_id = as.integer(study_id),
    
    # code book: 0 Male, 1 Female, 2 Other, 3 Not reported (only 0/1 occur
    # in this cohort; the other two levels are kept for correctness if a
    # future data pull includes them)
    sex = factor(sex, levels = c(0, 1, 2, 3),
                 labels = c("Male", "Female", "Other", "Not reported")),
    
    # code book: 0 Lymphoma, 1 Myeloma, 2 Leukemia (only 0 occurs - this is
    # a lymphoma-specific study cohort)
    diagnosis = factor(diagnosis, levels = c(0, 1, 2),
                       labels = c("Lymphoma", "Myeloma", "Leukemia")),
    
    # code book: 0 Non-Hodgkin's Lymphoma, 1 Other (specify in lym_other)
    lym_diagnosis = factor(lym_diagnosis, levels = c(0, 1),
                           labels = c("Non-Hodgkin lymphoma", "Other")),
    
    # code book: 0 DLBCL, 1 High-grade B-cell lymphoma, 2 Primary
    # mediastinal B-cell lymphoma, 3 Transformed DLBCL from follicular
    # lymphoma, 4 Follicular lymphoma, 5 Mantle cell lymphoma
    lym_nhl = factor(
      lym_nhl,
      levels = c(0, 1, 2, 3, 4, 5),
      labels = c(
        "DLBCL", "High-grade B-cell lymphoma", "Primary mediastinal B-cell lymphoma",
        "Transformed DLBCL from follicular lymphoma", "Follicular lymphoma",
        "Mantle cell lymphoma"
      )
    ),
    
    # rel_trans_yn - code book: 1 Yes, 0 No, 2 Unknown - a genuine 3-level field (unlike
    # most of the other *_yn fields below), only 0/1 observed here
    rel_trans_yn = recode_yn_unknown(rel_trans_yn),
    
    # cns_current - code book: 1 Yes, 0 No, 2 Unknown
    cns_current = recode_yn_unknown(cns_current),
    
    # cart_product - code book: 0 Kymriah, 1 Yescarta, 2 JCAR Trial, 3 Tecartus
    cart_product = factor(
      cart_product,
      levels = c(0, 1, 2, 3),
      labels = c("Kymriah", "Yescarta", "JCAR Trial", "Tecartus")
    ),
    
    # code book: 1 Yes, 0 No, for every field below (verified individually
    # against the code book; asct is listed there as "asct_yn" - same
    # field, exported under a shortened name)
    across(
      c(crs_yn, icans_yn, icu_yn, bridge_yn, asct, asct_relapse,
        active_mal, allosct_yn),
      recode_yn
    ),
    
    # code book: 1 Yes, 0 No ("Did the patient have any blood or metabolic
    # AEs?"). In this dataset the field only ever takes the value 1 - "No"
    # appears to have been left blank/missing rather than coded 0 - so in
    # practice every non-missing value below will read "Yes".
    ae_yn = recode_yn(ae_yn),
    
    # code book: Karnofsky is stored as a 1-10 INDEX (1 = 10%, 2 = 20%, ...,
    # 10 = 100%)
    karnofsky = as.integer(karnofsky) * 10L,
    
    # ECOG, line of therapy: code book confirms these are already the
    # direct numeric/ordinal value - no transformation needed
    ecog = as.integer(ecog),
    cart_line = as.integer(cart_line),
    
    # grade variables - already directly interpretable 0-4 scales per the
    # code book ("Text (number)", no separate label set)
    crs_highestgrade = as.integer(crs_highestgrade),
    icans_highest = as.integer(icans_highest),
    
    date_infusion = as.Date(date_infusion),
    los_days = as.numeric(los_days)
  )


# Bridge -----------------------------------

#   - bridge___0/1/2/3: code book confirms these are REDCap checkbox
#     indicators for bridging chemo (___0), radiation (___1) and steroids
#     (___2) respectively; ___3 is not defined in the code book at all and
#     is constant 0 (unused) in this cohort. Left as raw 0/1 - bridge_yn
#     and the named bridging-type fields already cover this; relabel at the
#     point of use if a single derived "bridging type" column is needed.


baseline_clean %>%
  select(contains("bridge")) %>% glimpse()


# recode bridege chemoreg
baseline_clean %>%
  count(bridge_chemoreg) %>%
  pull(1) %>% dput()

baseline_clean %>%
  mutate(
    bridge_chemoreg_clean = case_when(
      str_detect(bridge_chemoreg, regex("\\bpola", ignore_case = TRUE)) &
        str_detect(bridge_chemoreg, regex("\\bbr\\b", ignore_case = TRUE))  ~ "Polatuzumab BR",
      str_detect(bridge_chemoreg, regex("\\bpola", ignore_case = TRUE)) &
        str_detect(bridge_chemoreg, regex("\\br", ignore_case = T)) &
        !str_detect(bridge_chemoreg, regex("\\bbr\\b", ignore_case = TRUE)) ~ "Polatuzumab R",
      TRUE ~ bridge_chemoreg
    )) %>% count(bridge_chemoreg_clean, bridge_chemoreg) %>%
  arrange(pick(1), desc(n))

baseline_clean %>%
  mutate(across(bridge_chemoreg,
                function(x) { case_when(
                  str_detect(x, regex("\\bpola", ignore_case = TRUE)) &
                    str_detect(x, regex("\\bbr\\b", ignore_case = TRUE))  ~ "Polatuzumab BR",
                  str_detect(x, regex("\\bpola", ignore_case = TRUE)) &
                    str_detect(x, regex("\\br", ignore_case = T)) &
                    !str_detect(x, regex("\\bbr\\b", ignore_case = TRUE)) ~ "Polatuzumab R",
                  TRUE ~ x
                )})) %>%
  count(bridge_chemoreg, sort = T)


# change
baseline_clean <- baseline_clean %>%
  mutate(across(bridge_chemoreg,
                function(x) { case_when(
                  str_detect(x, regex("\\bpola", ignore_case = TRUE)) &
                    str_detect(x, regex("\\bbr\\b", ignore_case = TRUE))  ~ "Polatuzumab BR",
                  str_detect(x, regex("\\bpola", ignore_case = TRUE)) &
                    str_detect(x, regex("\\br", ignore_case = T)) &
                    !str_detect(x, regex("\\bbr\\b", ignore_case = TRUE)) ~ "Polatuzumab R",
                  TRUE ~ x
                )}))

baseline_clean %>%
  count(bridge_chemoreg)


## ---- Fields deliberately left un-recoded ---------------------------------
# Listed explicitly (rather than silently left alone) so it's clear this was
# a decision, not an oversight.
#   - bridge_chemoreg, bridge_steroids, active_mal_details, lym_other,
#     icu_reason_1/2: free-text fields per the code book - not reducible to
#     a factor.
baseline_clean %>%
  select(bridge_chemoreg, bridge_steroids, active_mal_details, lym_other,
         icu_reason_1, icu_reason_2) %>%
  glimpse() # they are all character or empty

baseline_clean %>%
  select(bridge_chemoreg, bridge_steroids, active_mal_details, lym_other,
         icu_reason_1, icu_reason_2) %>%
  map(., ~sort(table(.), decreasing = T))


#   - ALP/ALT/AST/anemia/hypocalcemia/hypokalemia/hyponatremia/
#     hypophosphatemia/INR/lymphocyte/neutrophil/platelet/PTT/WBC_highest:
#     lab-abnormality grade fields, code book confirms these are already
#     directly interpretable integer grades - no relabelling needed.

#   - cart_line: not defined in this code book at all (it predates the
#     field - cart_line first appears in the 10-Apr-2026 extract, not the
#     09-Jul-2025 one this code book documents). Plausibly related to the
#     code book's `tx_lines` ("number of prior lines of treatment") as
#     tx_lines + 1, but that field isn't present in this extract either, so
#     the relationship isn't verified - left as a plain integer.

# Code book fields NOT present in this extract at all (documented in the
# code book but not exported to this file - confirm with the study team if
# they're needed): education_years, race_Indigenous/asian/black/indian/
# latin/me/white/other, first_language, Gender (distinct from `sex`),
# tx_lines.

## ---- Sanity checks ---------------------------------------------------

stopifnot(
  "duplicate study_id in baseline_clean" = !anyDuplicated(baseline_clean$study_id),
  "date_infusion has NA after cleaning" = !anyNA(baseline_clean$date_infusion),
  "karnofsky out of the instrument's 10-100 range" =
    all(dplyr::between(baseline_clean$karnofsky, 10, 100), na.rm = TRUE)
)


message("--- Recoded factor levels (spot-check) ---")
message("sex:\n", paste(capture.output(print(table(baseline_clean$sex))), collapse = "\n"))
message("lym_diagnosis:\n", paste(capture.output(print(table(baseline_clean$lym_diagnosis))), collapse = "\n"))
message("rel_trans_yn:\n", paste(capture.output(print(table(baseline_clean$rel_trans_yn))), collapse = "\n"))
message("karnofsky (%):\n", paste(capture.output(print(table(baseline_clean$karnofsky))), collapse = "\n"))

message("\nRows: ", nrow(baseline_clean), " | Cols: ", ncol(baseline_clean))
message("Infusion date range: ", min(baseline_clean$date_infusion), " to ", max(baseline_clean$date_infusion))

## ---- Save ---------------------------------------------------------------

saveRDS(baseline_clean, here("data", "processed", "baseline_clean.rds"))

message("\n02_clean_baseline.R complete. Wrote data/processed/baseline_clean.rds")
