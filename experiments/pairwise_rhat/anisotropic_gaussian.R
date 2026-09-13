library(rstan)
library(mcmcConvergenceGraph)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path   <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/anisotropic_gaussian/anisotropic_gaussian.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/anisotropic_gaussian"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

result <- mcmcgraph(
  fit,
  parameters         = c("x[1]", "x[2]"),
  rho                = 1.05,
  plot               = TRUE,
  save_csv           = TRUE,
  save_plot          = TRUE,
  output_dir         = output_dir,
  pairwise_display_n = 10
)

print(result)
