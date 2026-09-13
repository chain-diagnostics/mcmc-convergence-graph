# Experiment 2, variant 2: different slopes.
# Data: two crossing Gaussian lines. Model: one Cauchy line y = a x + b.

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/cauchy_regression.stan"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/diff_slope_same_intercept"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

set.seed(2)
N <- 1000L
x <- runif(N, -1, 1)
y <- c(
  1 * x[1:500],
  -1* x[501:1000]
) + rnorm(N, 0, 0.05)

write.csv(
  data.frame(x = x, y = y, component = rep(1:2, each = 500)),
  file.path(output_dir, "data_generating_samples.csv"),
  row.names = FALSE
)

stan_data <- list(N = N, x = x, y = y)
saveRDS(stan_data, file.path(output_dir, "stan_data.rds"))

init_fn <- function() {
  list(
    a     = runif(1, -1, 1),
    b     = runif(1, -1, 1),
    gamma = runif(1, 0.04, 0.10)
  )
}

set.seed(2)
fit <- stan(
  file    = stan_file,
  data    = stan_data,
  chains  = 8,
  iter    = 2000,
  warmup  = 1000,
  seed    = 2,
  init    = init_fn
)

summary_matrix <- rstan::summary(fit)$summary
write.csv(summary_matrix, file.path(output_dir, "classical_rhat_summary.csv"), row.names = TRUE)
saveRDS(fit, file.path(output_dir, "diff_slope_same_intercept.rds"))
print(round(summary_matrix[c("a", "b", "gamma", "lp__"), ], 4))


traceplot(fit, pars = c("a", "b", "gamma"))

stan_hist(fit, pars = c("a", "b", "gamma"))

library("bayesplot")
library("rstanarm")
library("ggplot2")

posterior <- as.matrix(fit)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("a","b"),
           prob = 0.8) + plot_title

color_scheme_set("darkgray")
mcmc_scatter(
  as.matrix(fit),
  pars = c("a", "b"),
  np = nuts_params(fit),
  np_style = scatter_style_np(div_color = "green", div_alpha = 0.8)
)
