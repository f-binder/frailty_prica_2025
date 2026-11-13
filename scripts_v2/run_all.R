# ==============================================================================
# run_all.R - runs the whole v2 pipeline in order, each script in a clean
# environment (so no script silently depends on objects left by another).
#
#   Rscript scripts_v2/run_all.R          (from the project folder)
#
# Stops at the first error and reports which script failed. Takes ~1 minute
# (the trajectory models in 09 are the slowest step).
# ==============================================================================

scripts <- c(
  "01_import.R", "02_clean_derive.R", "03_descriptives.R", "04_feasibility.R",
  "05_correlations.R", "06_trajectories.R", "07_toxicity_los.R",
  "08_survival_baseline.R", "09_landmark_3mo.R", "10_figures.R", "12_alluvial_cfs.R",
  "11_check_abstract.R"
)

# start from a fresh key-results registry so stale numbers cannot survive a re-run
key_file <- here::here("output", "v2", "tables", "key_results.csv")
if (file.exists(key_file)) file.remove(key_file)

timings <- numeric()
for (s in scripts) {
  cat("\n>>> Running", s, "\n")
  t0 <- Sys.time()
  tryCatch(
    source(here::here("scripts_v2", s), local = new.env(), echo = FALSE),
    error = function(e) stop("Pipeline stopped in ", s, ": ", conditionMessage(e), call. = FALSE)
  )
  timings[s] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

cat("\nPipeline complete. Seconds per script:\n")
print(round(timings, 1))
cat("Outputs: output/v2/tables, output/v2/figures, output/v2/models; data: data/processed/v2\n")
