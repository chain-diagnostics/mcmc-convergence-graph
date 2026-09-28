library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "cauchy_regression.stan"
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

set.seed(2026)
N <- 300L
x <- runif(N, -1, 1)
y <- c(
  0.5 * x[1:150] + 0.25,
  -0.5 * x[151:300] - 0.25
) + rnorm(N, 0, 0.05)

write.csv(
  data.frame(x = x, y = y, component = rep(1:2, each = 150)),
  file.path(output_dir, "data_generating_samples.csv"),
  row.names = FALSE
)

stan_data <- list(N = N, x = x, y = y)
saveRDS(stan_data, file.path(output_dir, "stan_data.rds"))

init_fn <- function() {
  list(
    a     = runif(1, -3, 3),
    b     = runif(1, -3, 3),
    gamma = runif(1, 0, 1))

}

set.seed(1234)
fit <- stan(
  file    = stan_file,
  data    = stan_data,
  chains  = 8,
  iter    = 2000,
  warmup  = 1000,
  seed    = 1234,
  init    = init_fn
)

summary_matrix <- rstan::summary(fit)$summary
write.csv(summary_matrix, file.path(output_dir, "classical_rhat_summary.csv"), row.names = TRUE)
saveRDS(fit, file.path(output_dir, "diff_slope_diff_intercept.rds"))
print(round(summary_matrix[c("a", "b", "gamma", "lp__"), ], 4))
