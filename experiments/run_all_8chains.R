# Re-run all experiments with 8 chains.
# Order: install package -> baseline Stan fits -> pairwise diagnostics -> tables.

base_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/pairwiserhat/experiments"

models <- c(
  "uniform",
  "gaussian_ball",
  "three_cluster",
  "funnel",
  "linear_with_noise",
  "same_slope_diff_intercept",
  "diff_slope_same_intercept",
  "diff_slope_diff_intercept",
  "three_compartment_pk"
)

cat("=== Installing pairwiserhat from local source ===\n")
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools", repos = "https://cloud.r-project.org")
}
devtools::install(
  "/Users/chegu121/Documents/Phd-cici/r_package/pairwiserhat",
  upgrade = FALSE,
  quiet = TRUE
)

run_script <- function(path) {
  cat("\n########################################\n")
  cat("Running:", path, "\n")
  cat("########################################\n")
  start <- Sys.time()
  status <- system2("Rscript", args = path)
  elapsed <- round(difftime(Sys.time(), start, units = "mins"), 2)
  if (!identical(status, 0L)) {
    stop("Script failed (", status, "): ", path, call. = FALSE)
  }
  cat("Finished in", elapsed, "minutes\n")
}

for (m in models) {
  run_script(file.path(base_dir, "baseline_rhat", paste0(m, ".R")))
}

for (m in models) {
  run_script(file.path(base_dir, "pairwise_rhat", paste0(m, ".R")))
}

run_script(file.path(base_dir, "comparison_tables", "make_comparison_tables.R"))

cat("\n=== All experiments completed ===\n")
