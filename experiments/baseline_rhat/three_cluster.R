# three_cluster.R
#
# Simulates N iid draws from a three-component Gaussian mixture, saves
# that dataset, and fits three_cluster.stan. The diagnostic target is
# that same mixture, so multimodality lives in the known target, not in
# a mixture regression fitted to two-line data (Experiment 2).
#
# Runs with Stan's default random initialisation (Uniform(-2, 2) on the
# unconstrained scale). Starting points are not placed at known modes.
#
# Outputs (all in result_baseline_rhat/three_cluster/)
# ----------------------------------------------------
# data_generating_samples.csv    simulated mixture data
# stan_data.rds                  list(N, y1, y2) passed to Stan
# classical_rhat_summary.csv     rstan::summary() of the fit
# three_cluster.rds              the stanfit object

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/three_cluster.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/three_cluster"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cache_rds <- sub("\\.stan$", ".rds", stan_file)
if (file.exists(cache_rds)) file.remove(cache_rds)

mode_centres <- rbind(
  c(-6, -5),
  c( 6,  0),
  c( 0,  6)
)
component_sd <- 0.5

# --------------------------------------------------------------------
# 1. Simulate data from the three-component mixture
# --------------------------------------------------------------------

set.seed(2026)
N         <- 1000
component <- sample.int(3, N, replace = TRUE)
y1 <- mode_centres[component, 1] + rnorm(N, 0, component_sd)
y2 <- mode_centres[component, 2] + rnorm(N, 0, component_sd)

theta_dgp <- data.frame(
  theta1    = y1,
  theta2    = y2,
  component = component
)
write.csv(
  theta_dgp,
  file      = file.path(output_dir, "data_generating_samples.csv"),
  row.names = FALSE
)

stan_data <- list(N = N, y1 = y1, y2 = y2)
saveRDS(stan_data, file.path(output_dir, "stan_data.rds"))

# --------------------------------------------------------------------
# 2. Fit with Stan default random initialisation (no mode anchoring)
# --------------------------------------------------------------------

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

saveRDS(fit, file.path(output_dir, "three_cluster.rds"))

print(round(summary_matrix, 4))
