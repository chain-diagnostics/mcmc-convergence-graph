library(rstan)
library(pairwiserhat)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path   <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/two_cluster/two_cluster.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/two_cluster"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

result <- pairwiserhat(
  fit,
  rho                = 1.015,
  plot               = TRUE,
  save_csv           = TRUE,
  save_plot          = TRUE,
  output_dir         = output_dir,
  pairwise_display_n = 10
)

print(result)
