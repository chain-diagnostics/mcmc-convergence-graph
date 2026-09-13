# Probe whether more chains recover K = 3 under Stan default random init.
# Saves to separate folders so the n = 8 Experiment 1 results are untouched.

library(rstan)
library(mcmcConvergenceGraph)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file <- "/Users/chegu121/Documents/Phd-cici/r_package/stan_files/three_cluster.stan"
base_out  <- "/Users/chegu121/Documents/Phd-cici/r_package"

mode_centres <- rbind(
  c(-6, -5),
  c( 6,  0),
  c( 0,  6)
)
component_sd <- 0.5

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
stan_data <- list(N = N, y1 = y1, y2 = y2)

run_one <- function(n_chains) {
  tag <- paste0("three_cluster_n", n_chains)
  baseline_dir <- file.path(base_out, "result_baseline_rhat", tag)
  pairwise_dir <- file.path(base_out, "result_pairwise_rhat", tag)
  dir.create(baseline_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(pairwise_dir, recursive = TRUE, showWarnings = FALSE)

  write.csv(theta_dgp, file.path(baseline_dir, "data_generating_samples.csv"), row.names = FALSE)
  saveRDS(stan_data, file.path(baseline_dir, "stan_data.rds"))

  cat("\n=== Fitting three_cluster with n =", n_chains, "chains (init = random) ===\n")
  fit <- stan(
    file   = stan_file,
    data   = stan_data,
    chains = n_chains,
    iter   = 2000,
    warmup = 1000,
    seed   = 1234,
    init   = "random"
  )

  summary_matrix <- rstan::summary(fit)$summary
  write.csv(summary_matrix, file.path(baseline_dir, "classical_rhat_summary.csv"), row.names = TRUE)
  saveRDS(fit, file.path(baseline_dir, paste0(tag, ".rds")))
  print(round(summary_matrix[c("theta[1]", "theta[2]", "lp__"), c("mean", "n_eff", "Rhat"), drop = FALSE], 4))

  cat("\n=== Pairwise diagnostic for n =", n_chains, "===\n")
  result <- mcmcgraph(
    fit,
    parameters         = c("theta[1]", "theta[2]"),
    rho                = 1.05,
    plot               = TRUE,
    save_csv           = TRUE,
    save_plot          = TRUE,
    output_dir         = pairwise_dir,
    pairwise_display_n = 10
  )
  print(result)
  invisible(result)
}

for (n in c(16L, 24L)) {
  run_one(n)
}

cat("\n=== Done: three_cluster n = 16 and n = 24 ===\n")
