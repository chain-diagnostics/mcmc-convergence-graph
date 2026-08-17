# funnel.R
#
# Runs funnel.stan (2D) with 8 chains and draws iid samples from the
# 2-dimensional Neal's funnel using base R for the gray-dot layer of
# Fig. 1.
#
# Outputs (all in result_baseline_rhat/funnel/)
# ---------------------------------------------
# classical_rhat_summary.csv     rstan::summary() of the fit
# data_generating_samples.csv    iid samples (y, x) from the 2D funnel
# funnel.rds                     the stanfit object

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/funnel.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/funnel"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Drop the auto_write cache so the updated .stan is recompiled cleanly
cache_rds <- sub("\\.stan$", ".rds", stan_file)
if (file.exists(cache_rds)) file.remove(cache_rds)

# --------------------------------------------------------------------
# 1. iid samples from the 2D Neal's funnel
#    y ~ N(0, 3),   x | y ~ N(0, exp(y/2))
# --------------------------------------------------------------------

set.seed(2026)
n_iid <- 1000
y_iid <- rnorm(n_iid, 0, 3)
x_iid <- rnorm(n_iid, 0, exp(y_iid / 2))

theta_dgp <- data.frame(y = y_iid, x = x_iid)
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

saveRDS(fit, file.path(output_dir, "funnel.rds"))

print(round(summary_matrix, 4))
