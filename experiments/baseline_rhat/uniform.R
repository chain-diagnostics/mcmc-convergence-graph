
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/uniform.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/uniform"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

set.seed(2026)
n_iid <- 1000
theta_dgp <- data.frame(
  theta1 = runif(n_iid, -2, 2),
  theta2 = runif(n_iid, -2, 2)
)
write.csv(
  theta_dgp,
  file      = file.path(output_dir, "data_generating_samples.csv"),
  row.names = FALSE
)


fit <- stan(
  file   = stan_file,
  data   = list(),
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

saveRDS(fit, file.path(output_dir, "uniform.rds"))

print(round(summary_matrix, 4))
