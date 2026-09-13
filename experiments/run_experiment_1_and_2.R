# run_experiment_1_and_2.R
#
# Re-runs Experiment 1 and Experiment 2:
#   Exp 1: uniform, unimodal_gaussian, anisotropic_gaussian,
#          multimodal_gaussian (init_fn on [-1, 1]^2)
#   Exp 2: same_slope_diff_intercept, diff_slope_same_intercept,
#          diff_slope_diff_intercept (init_fn on [-1, 1])
#
# Order: install package -> baseline Stan fits -> pairwise diagnostics
#        -> comparison tables.

base_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/experiments"
log_file <- file.path(base_dir, "run_experiment_1_and_2.log")

models <- c(
  # Experiment 1
  "uniform",
  "unimodal_gaussian",
  "anisotropic_gaussian",
  "multimodal_gaussian",
  # Experiment 2
  "same_slope_diff_intercept",
  "diff_slope_same_intercept",
  "diff_slope_diff_intercept"
)

cat("=== Installing mcmcConvergenceGraph from local source ===\n")
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools", repos = "https://cloud.r-project.org")
}
devtools::install(
  "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph",
  upgrade = FALSE,
  quiet   = TRUE
)

run_script <- function(path) {
  cat("\n########################################\n")
  cat("Running:", path, "\n")
  cat("########################################\n")
  start  <- Sys.time()
  status <- system2("Rscript", args = path)
  elapsed <- round(difftime(Sys.time(), start, units = "mins"), 2)
  if (!identical(status, 0L)) {
    stop("Script failed (", status, "): ", path, call. = FALSE)
  }
  cat("Finished in", elapsed, "minutes\n")
}

cat("\n=== Baseline (Stan fits) ===\n")
for (m in models) {
  run_script(file.path(base_dir, "baseline_rhat", paste0(m, ".R")))
}

cat("\n=== Pairwise diagnostic ===\n")
for (m in models) {
  run_script(file.path(base_dir, "pairwise_rhat", paste0(m, ".R")))
}

run_script(file.path(base_dir, "comparison_tables", "make_comparison_tables.R"))

cat("\n=== Experiment 1 and 2 completed ===\n")
