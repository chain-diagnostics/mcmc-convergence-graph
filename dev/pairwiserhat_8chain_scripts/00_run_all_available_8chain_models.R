rm(list = ls())

# Master runner for the 8-chain scripts that were identifiable from the two uploaded files.
# I found 3 posterior scripts and 3 linear scripts in the uploaded code.
# Run one by one if you prefer, because each Stan model can take time.

script_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/pairwiserhat/dev/8chain_scripts"

scripts <- c(
  "01_run_posterior_B_line_with_noise_8chains.R",
  "02_run_posterior_C_funnel_8chains.R",
  "03_run_posterior_E_three_cluster_8chains.R",
  "04_run_linear_A_mix_same_a_diff_b_8chains.R",
  "05_run_linear_B_mix_same_b_diff_a_8chains.R",
  "06_run_linear_C_mix_diff_a_diff_b_8chains.R"
)

for (script in scripts) {
  cat("\n====================================\n")
  cat("Running script:\n")
  cat(script, "\n")
  cat("====================================\n")

  source(file.path(script_dir, script))
}
