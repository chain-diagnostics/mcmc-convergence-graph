library(rstan)
library(mcmcConvergenceGraph)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path   <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/same_slope_diff_intercept/same_slope_diff_intercept.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/same_slope_diff_intercept"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

# Cauchy fit to two-line data. Monitor a and b.
result <- mcmcgraph(
  fit,
  parameters                 = c("b","a"),
  rho                        = 1.05,
  plot                       = TRUE,
  save_csv                   = TRUE,
  save_plot                  = TRUE,
  output_dir                 = output_dir,
  pairwise_display_n         = 10,
  posterior_draws_parameters = c("a", "b"))
print(result)
