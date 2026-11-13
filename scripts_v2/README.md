# Frailty and CAR T — analysis pipeline v2

Reproduces every number in the EHA–EBMT CAR T meeting abstract (Nov 2026) from the
raw extract `data/raw/Frailty and CAR T - Frailty Data -10Apr2026 - v2 - nrm reviewed.xlsx`.
The v1 scripts (`scripts/`) are left untouched.

## Run

```bash
Rscript scripts_v2/run_all.R
```

About 30 seconds. Every script can also be run on its own (after the scripts before it
have been run once), e.g. `Rscript scripts_v2/08_survival_baseline.R`.

## Scripts

| Script | What it does |
|---|---|
| `00_setup.R` | Packages, paths, constants (data cut, landmark, adjustment set), plot theme, helpers |
| `01_import.R` | Reads the three sheets; CRP "<1.0" set to 0.1 |
| `02_clean_derive.R` | Code-book recoding; OS/PFS; cause of death (`reviewed_death_disease` supersedes `death_disease`); data-quality flags |
| `03_descriptives.R` | Table 1, events, causes of death, follow-up (reverse KM), OS/PFS, cumulative incidence of RM/NRM |
| `04_feasibility.R` | Completion of each test among attended visits (questionnaires only where the protocol required them); visit attendance; gait-missing vs CFS |
| `05_correlations.R` | Exploration (distributions, floor/ceiling), baseline Spearman matrix with CIs, cut-off agreement (kappa), correlations by visit, within-patient correlation, change correlation, CFS tracking |
| `06_trajectories.R` | Mixed models for repeated measures (heterogeneous compound symmetry), model-estimated means by visit |
| `07_toxicity_los.R` | Logistic models for CRS ≥2, ICANS, ICU; negative binomial models for length of stay |
| `08_survival_baseline.R` | Cox (univariable, adjusted for age + log2 LDH), Fine-Gray / cause-specific RM vs NRM, time-varying CFS |
| `09_landmark_3mo.R` | 3-month landmark: group-based trajectory model (lcmm, PROC TRAJ equivalent), CFS at 3 months, OS/PFS/RM/NRM |
| `10_figures.R` | Figures 1–4 (PNG 300 dpi, PDF, JPEG < 500 KB for submission) |
| `12_alluvial_cfs.R` | Figure 5: alluvial plot of CFS state and progression/death (or death) at each visit; transition table (runs before 11) |
| `11_check_abstract.R` | Compares all abstract numbers with the computed results (`output/v2/tables/abstract_check.csv`) |

## Outputs

- `output/v2/tables/` — all result tables; `key_results.csv` holds every abstract number
- `output/v2/figures/` — `fig1_feasibility_correlation`, `fig2_trajectories`, `fig3_landmark_trajectory_survival`
- `data/processed/v2/` — cleaned analysis datasets

`data/` and `output/` are excluded from git (patient data).

## Analysis decisions to know about

- **Cause of death**: 1 = relapse-related (RM), 0 = non-relapse (NRM). A standard-definition
  sensitivity analysis (death with vs without prior progression) is run alongside.
- **Adjusted models** use a pre-specified set (age + log2 LDH), not p-value selection.
- **Feasibility** = completion among *attended* visits. Attendance (attended / expected visits)
  is reported separately in `feasibility_attendance.csv`.
- **Open data issues** are listed in `output/v2/tables/data_quality_flags.csv`
  (e.g. one follow-up date after the data cut; PFS censored long after end of study).
