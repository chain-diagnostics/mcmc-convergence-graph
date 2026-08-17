library(rstan)
library(pairwiserhat)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path   <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/diff_slope_diff_intercept/diff_slope_diff_intercept.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/diff_slope_diff_intercept"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

result <- pairwiserhat(
  fit,
  rho                        = 1.015,
  plot                       = TRUE,
  save_csv                   = TRUE,
  save_plot                  = TRUE,
  output_dir                 = output_dir,
  pairwise_display_n         = 10,
  posterior_draws_parameters = c("a1", "b1")
)

print(result)
