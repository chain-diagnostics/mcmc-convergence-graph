library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/3pk_model/pairwise diagnostic/stan_file/three_compartment_pk_rate5_fixed_vc.stan"
data_file  <- "/Users/chegu121/Documents/Phd-cici/3pk_model/pairwise diagnostic/3pk_rate5_fixed_vc_results/remifentanil_3pk_stan_data.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/three_compartment_pk"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

stan_data <- readRDS(data_file)

fit <- stan(
  file   = stan_file,
  data   = stan_data,
  chains = 8,
  iter   = 2000,
  warmup = 1000,
  seed   = 1234,
  init   = "random"
)

summary_matrix <- rstan::summary(fit)$summary

write.csv(
  summary_matrix,
  file      = file.path(output_dir, "classical_rhat_summary.csv"),
  row.names = TRUE
)

saveRDS(fit, file.path(output_dir, "three_compartment_pk.rds"))

print(round(summary_matrix, 4))
