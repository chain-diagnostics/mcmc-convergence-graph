library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/diff_slope_same_intercept.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/diff_slope_same_intercept"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- stan(
  file   = stan_file,
  data   = list(N = 60L),
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

saveRDS(fit, file.path(output_dir, "diff_slope_same_intercept.rds"))

print(round(summary_matrix, 4))
