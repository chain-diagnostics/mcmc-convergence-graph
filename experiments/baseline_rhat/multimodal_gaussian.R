
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/multimodal_gaussian.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/multimodal_gaussian"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

init_fn <- function() list(x = runif(2, -1, 1))

fit <- stan(
  file    = stan_file,
  data    = list(),
  chains  = 8,
  warmup  = 1000,
  iter    = 2000,
  seed    = 123,
  init    = init_fn
)

summary_matrix <- rstan::summary(fit)$summary
write.csv(
  summary_matrix,
  file = file.path(output_dir, "classical_rhat_summary.csv"),
  row.names = TRUE
)
saveRDS(fit, file.path(output_dir, "multimodal_gaussian.rds"))
print(round(summary_matrix, 4))
