library(rstan)
library(mcmcConvergenceGraph)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path   <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/three_compartment_pk_infer_vc/three_compartment_pk_infer_vc.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/three_compartment_pk"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

rate_params <- c("k10", "k12", "k21", "k13", "k31","VC")

result <- mcmcgraph(
  fit,
  parameters         = c("k12"),
  rho                = 1.05,
  plot               = TRUE,
  save_csv           = TRUE,
  save_plot          = TRUE,
  output_dir         = output_dir,
  pairwise_display_n = 10
)

print(result)
