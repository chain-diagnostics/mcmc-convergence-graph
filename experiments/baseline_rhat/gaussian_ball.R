# gaussian_ball.R
#
# Runs gaussian_ball.stan with 8 chains and draws iid samples from
# N(0, I_2) for the gray-dot layer of Fig. 1.
#
# Outputs (all in result_baseline_rhat/gaussian_ball/)
# ----------------------------------------------------
# classical_rhat_summary.csv     rstan::summary() of the fit
# data_generating_samples.csv    iid samples from N(0, I_2)
# gaussian_ball.rds              the stanfit object

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/gaussian_ball.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/gaussian_ball"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --------------------------------------------------------------------
# 1. iid samples from the data-generating distribution
# --------------------------------------------------------------------

set.seed(2026)
n_iid <- 1000
theta_dgp <- data.frame(
  theta1 = rnorm(n_iid, 0, 1),
  theta2 = rnorm(n_iid, 0, 1)
)
write.csv(
  theta_dgp,
  file      = file.path(output_dir, "data_generating_samples.csv"),
  row.names = FALSE
)

# --------------------------------------------------------------------
# 2. Fit the Stan model
# --------------------------------------------------------------------

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

saveRDS(fit, file.path(output_dir, "gaussian_ball.rds"))

print(round(summary_matrix, 4))
