# run_experiment_1.R
#
# Re-runs the four Experiment 1 targets end-to-end:
#   uniform, unimodal_gaussian, anisotropic_gaussian, multimodal_gaussian
#
# For each target this script executes
#   (a) baseline_rhat/<target>.R     (Stan fit; no dummy data)
#   (b) pairwise_rhat/<target>.R     (MCMC convergence graph diagnostic)
#
# Each script is launched in a fresh Rscript session, matching the
# pattern used by run_all_8chains.R.

base_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/experiments"

models <- c(
  "uniform",
  "unimodal_gaussian",
  "anisotropic_gaussian",
  "multimodal_gaussian"
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
  start   <- Sys.time()
  status  <- system2("Rscript", args = path)
  elapsed <- round(difftime(Sys.time(), start, units = "mins"), 2)
  if (!identical(status, 0L)) {
    stop("Script failed (", status, "): ", path, call. = FALSE)
  }
  cat("Finished in", elapsed, "minutes\n")
}

cat("\n=== Baseline (Stan fits + iid samples) ===\n")
for (m in models) {
  run_script(file.path(base_dir, "baseline_rhat", paste0(m, ".R")))
}

cat("\n=== Pairwise diagnostic ===\n")
for (m in models) {
  run_script(file.path(base_dir, "pairwise_rhat", paste0(m, ".R")))
}

cat("\n=== Experiment 1 completed ===\n")
