# three_cluster.R
#
# Runs three_cluster.stan with 8 chains under Stan's default random
# initialisation (Uniform(-2, 2) on the unconstrained scale). Starting
# points are not placed at known modes.
#
# Outputs (all in result_baseline_rhat/three_cluster/)
# ----------------------------------------------------
# classical_rhat_summary.csv     rstan::summary() of the fit
# data_generating_samples.csv    iid samples from the mixture
# three_cluster.rds              the stanfit object

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/three_cluster.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/three_cluster"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

mode_centres <- rbind(
  c(-6, -5),
  c( 6,  0),
  c( 0,  6)
)
component_sd <- 0.5

# --------------------------------------------------------------------
# 1. iid samples from the mixture (gray dots for Fig. 1)
# --------------------------------------------------------------------

set.seed(2026)
n_iid     <- 1000
component <- sample.int(3, n_iid, replace = TRUE)
theta_dgp <- data.frame(
  theta1    = mode_centres[component, 1] + rnorm(n_iid, 0, component_sd),
  theta2    = mode_centres[component, 2] + rnorm(n_iid, 0, component_sd),
  component = component
)
write.csv(
  theta_dgp,
  file      = file.path(output_dir, "data_generating_samples.csv"),
  row.names = FALSE
)

# --------------------------------------------------------------------
# 2. Fit with Stan default random initialisation (no mode anchoring)
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

saveRDS(fit, file.path(output_dir, "three_cluster.rds"))

print(round(summary_matrix, 4))
