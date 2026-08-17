# linear_with_noise.R
#
# Generates a small linear regression dataset in R, fits
# linear_with_noise.stan with 8 chains, and saves the same dataset for
# the gray-dot layer of Fig. 1 (which for this panel IS the observed
# data, not a separate iid draw from a target density).
#
# Outputs (all in result_baseline_rhat/linear_with_noise/)
# --------------------------------------------------------
# classical_rhat_summary.csv     rstan::summary() of the fit
# data_generating_samples.csv    the (x_n, y_n) pairs used in the fit
# linear_with_noise.rds          the stanfit object

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/linear_with_noise.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/linear_with_noise"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Drop the auto_write cache so the updated .stan is recompiled cleanly
cache_rds <- sub("\\.stan$", ".rds", stan_file)
if (file.exists(cache_rds)) file.remove(cache_rds)

# --------------------------------------------------------------------
# 1. Draw the dataset from the true data-generating distribution:
#      x_n ~ Uniform(-2, 2)
#      y_n | x_n ~ N(1.5 * x_n + 0.5, 0.5^2)
# --------------------------------------------------------------------

set.seed(2026)
N <- 60
x <- runif(N, -2, 2)
y <- 1.5 * x + 0.5 + rnorm(N, 0, 0.5)

theta_dgp <- data.frame(x = x, y = y)
write.csv(
  theta_dgp,
  file      = file.path(output_dir, "data_generating_samples.csv"),
  row.names = FALSE
)

# --------------------------------------------------------------------
# 2. Fit the Stan model on the generated data
# --------------------------------------------------------------------

stan_data <- list(N = N, x = x, y = y)

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

saveRDS(fit, file.path(output_dir, "linear_with_noise.rds"))

print(round(summary_matrix, 4))
